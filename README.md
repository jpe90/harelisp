# WIP lisp written in Hare.

- [x] cons
- [x] car
- [x] cdr
- [x] quote
- [x] define
- [x] \+
- [x] \-
- [ ] \*
- [ ] /
- [ ] >
- [ ] <
- [x] lambda
- [x] list
- [x] setq
- [x] macroexpand
- [x] if
- [x] =
- [x] defmacro
- [x] defun
- [ ] gensym
- [ ] garbage collection

## Usage

```
[solaire@Hyperion hare-lisp]$ hare run lisp.ha
> (cons 1 2)
(1 . 2)
> (define x (lambda (y) (+ 2 y)))
Function
> (x 7)
9
```
