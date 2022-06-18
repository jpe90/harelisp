use fmt;
use os;
use bufio;
use unix::tty;
use io;
use strings;
use bytes;
use ascii;
use encoding::utf8;
use strio;

// ===========================================================================
//                                Lisp Types

type Nil = void;
type Cparen = void;
type Dot = void;
type True = void;
type EOF = void;
type Value = int;
type Symbol = str;

type Cell = struct {
	car: Obj,
	cdr: Obj
};

type List = (*Cell | Nil);

type Primitive = *fn(_: *Environment,_: List) (Obj | invocation_error);

type Environment = struct {
	vars: List,
	up: (*Environment | Nil),
};

type Fntype = enum {
	FUNCTION,
	MACRO
};

type Function = struct {
	params: List,
	body: List,
	env: *Environment,
};

type Macro = struct {
	params: List,
	body: List,
	env: *Environment,
};

type Obj = (*Value | *Cell | *Symbol | Primitive | *Function | *Macro | Nil
	| Cparen | Dot | True | EOF);

// ===========================================================================
//                                Error Types

type unclosedparens = !void;
type straydot = !void;
type unknown = !void;
type malformed = !void;
type notfound = !void;
type bad_args = !void;
type unbound_variable = !void;
type mismatched_args = !void;
type readerror = !(unclosedparens | straydot | unknown);
type scanerror = !(utf8::invalid | io::error);
type invocation_error = !(malformed | unbound_variable | bad_args
	| div_by_zero);
type notdigit = void;
type div_by_zero = !void;

// ===========================================================================
//                              Global Variables

let tty: io::file = 0;
let rbuf: [os::BUFSIZ]u8 = [0...];
let wbuf: [os::BUFSIZ]u8 = [0...];
let symbols: Obj = Nil;
let buftty_bufio: bufio::bufstream = bufio::bufstream  {
    source = 0,
    ...
};

// ===========================================================================
//                                  Functions

fn list_length(list: Obj) int = {
	let length: int = 0;
	for(true) {
		match(list) {
			case let c: *Cell => {
				list = c.cdr;
				length = length + 1;
			};
			case Nil =>
				return length;
			case => {
				length = length + 1;
				return length;
			};
		};
	};
	return length;
};

@test fn list_length() void = {
	let obj1: Obj = alloc(1);
	let obj2: Obj = alloc(2);
	let mycell = cons(obj1,obj2);
	assert(list_length(mycell) == 2);
};

fn cons(car: Obj, cdr: Obj) *Cell = {
	return alloc(Cell {
		car = car,
		cdr = cdr,
	});
};

@test fn cons() void = {
	let one: Obj = alloc(1);
	let cell = cons(one,Nil);
	assert(cell.cdr is Nil);
	assert(list_length(cell) == 1);

	let two: Obj = alloc(2);
	let another_cell = cons(two,one);
	assert(list_length(another_cell) == 2);
};

fn acons(x: Obj, y: Obj, a: Obj) Obj = {
	return cons(cons(x,y),a);
};

fn skip_line() (void | scanerror) = {
	for (true) {
		let letter = bufio::scanrune(os::stdin)!;
		if(!(letter is rune)) {
			continue;
		};
		let rn = letter as rune;
		if (rn == '\n')
			return;
		if (rn == '\r') {
			match (peek_rune()) {
			case let rn: rune =>
				if (rn != '\n') {
					bufio::scanrune(os::stdin)!;
				};
			case let e: scanerror => return e;
			case => return;
			};
			return;
		};
	};
	return;
};

// this will push runes back on standard input but will consume EOF
fn peek_rune() ( rune | io::EOF | scanerror ) = {
	let letter = bufio::scanrune(os::stdin)!;
	if(letter is io::EOF)
		return io::EOF;
	bufio::unreadrune(os::stdin,letter as rune);
	return letter;
};

// this will push runes back on standard input but will consume EOF
fn check_for_digit() ( u32 | notdigit | io::EOF | scanerror ) = {
	const scan = bufio::scanrune(os::stdin)!;
	match(scan) {
		case io::EOF =>
			return io::EOF;
		case let rn: rune =>
			if(ascii::isdigit(rn)) {
				return rn: u32;
			}
			else {
				bufio::unreadrune(os::stdin,rn);
				return notdigit;
			};
	};
};

fn read_number(val: int) int = {
	for(true) {
		let next_rune = peek_rune()!;
		if(next_rune is io::EOF)
			return val;
		let rn = next_rune as rune;
		if(ascii::isdigit(rn)) {
			let next_digit = (bufio::scanrune(os::stdin)! as rune);
			let next_int = next_digit: u32 - '0';
			val = val * 10 + (next_int: int);
		}
		else
			return val;
	};
	return val;
};

fn read_list() (Obj | readerror) = {
    let ob = read();
	match (ob) {
	case let re: readerror =>
		return re;
	case EOF =>
		return unknown;
	case Dot =>
		return straydot;
	case Cparen =>
		return Nil;
	case =>
		yield;
	};
	let ob = ob as Obj;
	let head = cons(ob,Nil);
	let tail = head;

	for(true) {
        ob = read()!;
		match (ob) {
		case EOF =>
			return unclosedparens;
		case Cparen =>
			return head;
		case Dot =>
			tail.cdr = read()!;
			let close: Obj = read()!;
			assert(close is Cparen);
			return head;
		case =>
			yield;
		};
		tail.cdr = cons(ob, Nil);
		tail = (tail.cdr as *Cell);
	};
	return unknown;
};

fn read_quote() Obj = {
    let sym: Obj = intern("quote");
    return cons(sym, cons(read()!, Nil));
};

fn read_symbol(alnum: rune) Obj = {
	let runes: []rune = [];
	append(runes,alnum);
	for(true) {
		const rn = peek_rune()!;
		match(rn) {
			case let r: rune =>
			if(ascii::isalnum(r) || r == '-') {
				const rx = bufio::scanrune(os::stdin)! as rune;
				append(runes,rx);
			} else {
				break;
			};
			case io::EOF =>
				break;
		};
	};
	return alloc(strings::fromrunes(runes));
};

fn print(obj: Obj) void = {
	match(obj) {
		case let v: *Value =>
			fmt::printf("{}", *v: int)!;
		case let s: *Symbol =>
			fmt::printf("{}", *s: str)!;
		case let s: *Cell => {
			fmt::print("(")!;
			for(true) {
				print(s.car);
				if(s.cdr is Nil)
					break;
				if(!(s.cdr is *Cell)) {
						fmt::print(" . ")!;
						print(s.cdr);
						break;
				};
				fmt::print(" ")!;
				s = (s.cdr as *Cell);
			};
			fmt::printf("{}",")")!;
		};
		case let s: Primitive =>
			fmt::print("Primitive")!;
		case let s: *Function =>
			fmt::print("Function")!;
		case let s: *Macro =>
			fmt::print("Macro")!;
		case Nil =>
			fmt::print("Nil")!;
		case True =>
			fmt::print("True")!;
		case EOF =>
    		return;
		case =>
			fmt::print("Error")!;
	};
};

fn read() (Obj | readerror) = {
	for (true) {
		let letter = bufio::scanrune(os::stdin)!;
		match (letter) {
		case let res: rune =>
			if (res == ' ' || res == '\n' || res == '\r' || res == '\t')
				continue;
			if (res == ';') {
				skip_line()!;
				continue;
			};
			if (res == '(')
				return read_list();
			if (res == ')')
				return Cparen;
			if (res == '.')
				return Dot;
			if (res == '\'')
				return read_quote();
			if (ascii::isdigit(res)) {
				const uval = res: u32;
				return alloc(read_number(uval: int - '0'));
			};
			if (res == '-') {
				const next = check_for_digit();
				match(next) {
					case let i: u32 =>
						return alloc(-1 * read_number(i: int - '0'));
					case =>
						return read_symbol(res);
				};
			};
			if (ascii::isalpha(res) || strings::contains("+=!@#$%^&*/<>",res))
				return read_symbol(res);
			return unknown;
		case io::EOF =>
			return EOF;
		case =>
			fmt::fatal("unreachable");
		};
	};
	return EOF;
};

fn intern(name: str) *Symbol = {
	for(let p = symbols; !(p is Nil); p = (p as *Cell).cdr) {
		let stp = (p as *Cell).car as *Symbol;
		if(strings::compare(*stp: str, name) == 0) {
    		return stp;
		};
	};

	let sym: *Symbol = alloc(name);
	let new_symbols = cons(sym,symbols);
	symbols = new_symbols;

	return sym;
};

@test fn intern() void = {
	intern("one");
	intern("two");
	intern("one");
	assert(list_length(symbols) == 2);
};


fn add_variable(env: *Environment, sym: Obj, val: Obj) void = {
	env.vars = acons(sym, val, env.vars) as List;
};

fn print_vars_in_env(env: (*Environment | Nil)) void = {
	let iter = env;
	for(true) {
		let cell = (iter as *Environment).vars as *Cell;
		for(true) {
			let bind = cell.car as *Cell;
			let sym = bind.car as *Symbol;
			let symstr = *sym: str;
			match (cell.cdr) {
				case let c:*Cell =>
					cell = c;
				case Nil =>
					break;
				case =>
					break;
			};
		};
		if((iter as *Environment).up is *Environment) {
			iter = (iter as *Environment).up;
		}
		else
			return;
	};
};

fn push_env(env: *Environment, vars: List, values: List) (*Environment
	| mismatched_args) = {
	if(list_length(vars) != list_length(values))
		return mismatched_args;
	let map: Obj = Nil;
	for(let p = vars, q = values; !(p is Nil)) {
		let sym = (p as *Cell).car;
		let val = (q as *Cell).car;
		map = acons(sym,val,map);
		p = (p as *Cell).cdr as List;
		q = (q as *Cell).cdr as List;
	};
	return alloc(Environment {
		vars = map as List,
		up = env
	});
};

fn progn(env: *Environment, list: List) Obj = {
	let r: Obj = Nil;
	for (let lp = list; !(lp is Nil); lp = (lp as *Cell).cdr as List) {
		r = eval(env, (lp as *Cell).car);
	};
	return r;
};

fn compare_symbols(a: *Symbol, b: *Symbol) bool = {
	return strings::compare(*a: str, *b: str) == 0;
};

fn find(env: (*Environment | Nil), to_find: *Symbol) (*Cell | notfound) = {
	for(let iter = env; iter is *Environment; iter = (iter as *Environment).up) {
		if((iter as *Environment).vars is Nil)
			continue;
		let cell = (iter as *Environment).vars as *Cell;
		for(true) {
			let bind = cell.car as *Cell;
			let sym = bind.car as *Symbol;
  			if(compare_symbols(to_find,sym))
  				return bind;
			match (cell.cdr) {
				case let c:*Cell =>
					cell = c;
				case =>
					break;
			};
		};
	};
	return notfound;
};

fn should_expand(env: *Environment, obj: Obj) bool = {
	if(!(obj is *Cell))
		return false;
	const cobj = (obj as *Cell);
	if(!(cobj.car is *Symbol))
		return false;
	const sym = (cobj.car as *Symbol);
	const bind = find(env,sym);
	if(bind is notfound)
		return false;
	const bind = bind as *Cell;
	if(!(bind.cdr is *Macro))
		return false;
	return true;
};

fn macroexpand(env: *Environment, obj: Obj) Obj = {
	if(!should_expand(env,obj))
		return obj;
	const cobj = (obj as *Cell);
	const sym = (cobj.car as *Symbol);
	const bind = find(env,sym) as *Cell;
	const macro = bind.cdr as *Macro;
	const args = cobj.cdr as List;
	const body = macro.body;
	const params = macro.params as *Cell;
	const newenv = push_env(env,params,args)!;
	return progn(newenv,body);
};

fn handle_defun(env: *Environment, list: List, fntype: Fntype) (Obj |
	invocation_error) = {
	if(list is Nil)
		return malformed;
	let list_cell = list as *Cell;
	if(!(list_cell.car is *Symbol) || !(list_cell.cdr is *Cell))
		return malformed;
	const sym = list_cell.car as *Symbol;
	const cell = list_cell.cdr as *Cell;
	const fnobj = handle_function(env,cell,fntype)!;
	add_variable(env,sym,fnobj);
	return fnobj;
};

fn eval(env: *Environment, obj: Obj) Obj = {
	match(obj) {
		case let v: (*Value | *Function | Primitive | Nil | Dot | Cparen | True) =>
			return v;
		case let s: *Symbol =>
			let bind = find(env, s);
			match(bind) {
				case let cell: *Cell =>
					return cell.cdr;
				case notfound =>
					return Nil;
			};
		case let s: *Cell =>
        	if(should_expand(env, obj))
        		return eval(env,macroexpand(env,obj));
			let funsym = s.car;
			let fun = eval(env,funsym);
			let args = s.cdr as List;
			return apply(env,fun as (Primitive | *Function),args);
		case =>
			return EOF;
	};
};

fn apply(env: *Environment, function: (Primitive | *Function), args: List) Obj = {
	match(function) {
		case let p: Primitive =>
    		return p(env, args)!;
		case let f: *Function => {
			let body = f.body;
			let params = f.params;
			let eargs = eval_list(env,args);
			let newenv = push_env(f.env, params, eargs as List)!;
			return progn(newenv, body);
		};
	};
};


fn handle_function(env: *Environment, list: List, fntype: Fntype) (Obj |
	invocation_error) = {
	if(!((list as *Cell).car is *Cell) || !((list as *Cell).cdr is *Cell)) {
		fmt::println("lambda is malformed")!;
		return malformed;
	};
	for(let p = (list as *Cell).car; !(p is Nil); p = (p as *Cell).cdr) {
		if(!((p as *Cell).car is *Symbol))
			return malformed;
	};
	let car = (list as *Cell).car;
	let cdr = (list as *Cell).cdr;
	if (fntype == Fntype::FUNCTION)
		return alloc(Function {
			params = car as List,
			body = cdr as List,
			env = env})
	else
		return alloc(Macro {
			params = car as List,
			body = cdr as List,
			env = env});
};


fn eval_list(env: *Environment, list: List) Obj = {
	let tail: Obj = Nil;
	let head: Obj = Nil;
	if (list is Nil)
		return Nil;
	for(let p = list; !(p is Nil); p = (p as *Cell).cdr as List) {
		let tmp: Obj = eval(env, (p as *Cell).car);
		if(head is Nil) {
			tail = cons(tmp,Nil);
			head = tail;
		} else {
			let new_tail_cdr = cons(tmp, Nil);
			(tail as *Cell).cdr = new_tail_cdr;
			tail = new_tail_cdr;
		};
	};
	return head;
};

// ===========================================================================
//                                  Primitives

fn prim_cons(env: *Environment, list: List) (Obj | invocation_error) = {
	if(list_length(list) != 2)
		return malformed;
	let list = list as *Cell;
	let cell = eval_list(env, list) as *Cell;
	let cell_cdr = cell.cdr as *Cell;
	cell.cdr = cell_cdr.car;
	return cell;
};
fn prim_car(env: *Environment, list: List) (Obj | invocation_error) = {
	const args = eval_list(env, list);
	if(!(args is *Cell))
		return malformed;
	const args = args as *Cell;
		if(!(args.car is *Cell))
			return malformed;
	return (args.car as *Cell).car;
};

fn prim_cdr(env: *Environment, list: List) (Obj | invocation_error) = {
	const args = eval_list(env, list);
	if(!(args is *Cell))
		return malformed;
	const args = args as *Cell;
		if(!(args.car is *Cell))
			return malformed;
	return (args.car as *Cell).cdr;
};


fn prim_if(env: *Environment, list: List) (Obj | invocation_error) = {
	if(list_length(list) < 2)
		return malformed;
	const cell = list as *Cell;
	const cond = eval(env, cell.car);
	const cell_cdr = cell.cdr as *Cell;
	match(cond) {
		case Nil =>
			const els = cell_cdr.cdr;
			if(els is Nil)
				return Nil
			else
				// sketch
				return progn(env, els as List);
		case =>
			const then = cell_cdr.car;
			return eval(env,then);
	};
};


fn prim_lambda(env: *Environment, list: List) (Obj | invocation_error) = {
	return handle_function(env,list,Fntype::FUNCTION);
};

fn prim_list(env: *Environment, list: List) (Obj | invocation_error) = {
	return eval_list(env,list);
};

fn prim_setq(env: *Environment, list: List) (Obj | invocation_error) = {
	if(list_length(list) != 2)
		return malformed;
	let cell = list as *Cell;
	if(!(cell.car is *Symbol))
		return malformed;
	let bind = find(env,cell.car as *Symbol);
	if (bind is notfound)
		return unbound_variable;
	let val = eval(env, (cell.cdr as *Cell).car);
	(bind as *Cell).cdr = val;
	return val;
};

fn prim_macroexpand(env: *Environment, list: List) (Obj | invocation_error) = {
	if(list is Nil)
		return malformed;
	list = list as *Cell;
	if(list_length(list) != 1)
		return malformed;
	if(!(list is *Cell))
		return malformed;
	const body = (list as *Cell).car;
	return macroexpand(env, body);
};

fn prim_defmacro(env: *Environment, list: List) (Obj | invocation_error) = {
	return handle_defun(env, list, Fntype::MACRO);
};

fn prim_defun(env: *Environment, list: List) (Obj | invocation_error) = {
	return handle_defun(env, list, Fntype::FUNCTION);
};

fn prim_quote(env: *Environment, list: List) (Obj | invocation_error) = {
	match(list) {
	case Nil =>
		return malformed;
	case let c: *Cell =>
		if(list_length(c) != 1)
			return malformed
		else
			return c.car;
	};
};

fn prim_plus(env: *Environment, list: List) (Obj | invocation_error) = {
	let sum: int = 0;
	for(let args= eval_list(env,list); !(args is Nil); args = (args as *Cell).cdr
		as List) {
		if(!((args as *Cell).car is *Value)) {
			return malformed;
		};
		sum = sum + *((args as *Cell).car as *Value): int;
	};
	return alloc(sum);
};

fn prim_times(env: *Environment, list: List) (Obj | invocation_error) = {
	let sum: int = 1;
	for(let args= eval_list(env,list); !(args is Nil); args = (args as *Cell).cdr
		as List) {
		if(!((args as *Cell).car is *Value)) {
			return malformed;
		};
		sum = sum * *((args as *Cell).car as *Value): int;
	};
	return alloc(sum);
};

fn prim_minus(env: *Environment, list: List) (Obj | invocation_error) = {
	let args = eval_list(env,list);
	if(!((args as *Cell).car is *Value)) {
		return malformed;
	};
	let r = *((args as *Cell).car as *Value): int;
	args = (args as *Cell).cdr as List;
	for(!(args is Nil); args = (args as *Cell).cdr as List) {
		if(!((args as *Cell).car is *Value)) {
			return malformed;
		};
		const next_r = *((args as *Cell).car as *Value): int;
		r = r - next_r;
	};
	return alloc(r);
};

fn prim_div(env: *Environment, list: List) (Obj | invocation_error) = {
	let args = eval_list(env,list);
	if(!((args as *Cell).car is *Value)) {
		return malformed;
	};
	let r = *((args as *Cell).car as *Value): int;
	args = (args as *Cell).cdr as List;
	for(!(args is Nil); args = (args as *Cell).cdr as List) {
		if(!((args as *Cell).car is *Value)) {
			return malformed;
		};
		const next_r = *((args as *Cell).car as *Value): int;
		// fmt::println(next_r)!;
		if (next_r == 0)
			return div_by_zero;
		r = r / next_r;
	};
	return alloc(r);
};

fn prim_lt(env: *Environment, list: List) (Obj | invocation_error) = {
	let args = eval_list(env,list);
	if(list_length(args) != 2)
		return malformed;
	const x = (args as *Cell).car;
	const y_cell = (args as *Cell).cdr;
	const y = (y_cell as *Cell).car;
	if(!(x is *Value && y is *Value))
		return malformed;
	const x_num = *(x as *Value): int;
	const y_num = *(y as *Value): int;
	if(x_num < y_num)
		return True
	else
		return Nil;
};

fn prim_gt(env: *Environment, list: List) (Obj | invocation_error) = {
	let args = eval_list(env,list);
	if(list_length(args) != 2)
		return malformed;
	const x = (args as *Cell).car;
	const y_cell = (args as *Cell).cdr;
	const y = (y_cell as *Cell).car;
	if(!(x is *Value && y is *Value))
		return malformed;
	const x_num = *(x as *Value): int;
	const y_num = *(y as *Value): int;
	if(x_num > y_num)
		return True
	else
		return Nil;
};

fn prim_num_eq(env: *Environment, list: List) (Obj | invocation_error) = {
	if(list_length(list) != 2)
		return malformed;
	const list = list as *Cell;
	const values = eval_list(env, list) as *Cell;
	const x = values.car;
	const cdr = (values.cdr) as *Cell;
	const y = cdr.car;
	if(!(x is *Value && y is *Value))
		return malformed;
	const x_int = *(x as *Value): int;
	const y_int = *(y as *Value): int;
	if(x_int == y_int)
		return True
	else
		return Nil;
};

fn prim_define(env: *Environment, list: List) (Obj | invocation_error) = {
	let sum: int = 0;
	if(list_length(list) != 2)
		return malformed;
	let cell = list as *Cell;
	if(!(cell.car is *Symbol))
		return malformed;
	let sym = cell.car;
	let value_cell = cell.cdr as *Cell;
	let value = eval(env,value_cell.car);
	add_variable(env,sym,value);
	return value;
};

fn add_primitive(env: *Environment, name: str, fun: Primitive) void = {
	let sym = intern(name);
	add_variable(env, sym, fun);
};

fn define_primitives(env: *Environment) void = {
	add_primitive(env, "cons", &prim_cons);
	add_primitive(env, "car", &prim_car);
	add_primitive(env, "cdr", &prim_cdr);
	add_primitive(env, "quote", &prim_quote);
	add_primitive(env, "define", &prim_define);
	add_primitive(env, "+", &prim_plus);
	add_primitive(env, "-", &prim_minus);
	add_primitive(env, "*", &prim_times);
	add_primitive(env, "/", &prim_div);
	add_primitive(env, "<", &prim_lt);
	add_primitive(env, ">", &prim_gt);
	add_primitive(env, "lambda", &prim_lambda);
	add_primitive(env, "list", &prim_list);
	add_primitive(env, "setq", &prim_setq);
	add_primitive(env, "macroexpand", &prim_macroexpand);
	add_primitive(env, "if", &prim_if);
	add_primitive(env, "=", &prim_num_eq);
	add_primitive(env, "defun", &prim_defun);
	add_primitive(env, "defmacro", &prim_defmacro);
};

// ===========================================================================
//                                   Entry Point

export fn main() void = {
	let env: *Environment = &Environment {
	    vars = Nil,
	    up = Nil
	};
	define_primitives(env);
	for (true) {
		fmt::print("> ")!;
		bufio::flush(os::stdout)!;
		let expr = read();
		match (expr){
			case unclosedparens =>
				fmt::println("Found unclosed parenthesis")!;
			case straydot =>
				fmt::println("Found a stray . ")!;
			case unknown =>
				fmt::println("Haven't made a good error message for this yet :^)")!;
			case =>
				let expr = expr as Obj;
				print(eval(env,expr));
		};
		fmt::printf("\n")!;
	};
};
