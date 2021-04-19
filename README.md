# scanfcl

An implementation of `sscanf` in Common Lisp.

## Quickstart

Clone this respository into your `quicklisp/local-projects` directory, then `(ql:quickload "scanfcl")`, and swtich into the `SCANFCL` package.

```lisp
SCANFCL> (sscanf "123 abc" "%d %s")

(123 "abc")

SCANFCL> (let ((scanner (compile-control-string "%d %s")))
            (sscanf "123 abc" scanner))

(123 "abc")

SCANFCL> (sscanf "-INFINITY NAN 0xDEAD.BEEFpFF" "%Lf %f %lf")

(#.SB-EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY #<SINGLE-FLOAT quiet NaN> 57005.745834350586d0)
```

## Introduction

Sometimes you just want to reproduce the effect of `C`-style `scanf` input without having to write your own parser. **scanfcl** reproduces the effect of `sscanf` conversion from lisp strings to lisp objects. The standard 'C99' conversion specifiers are supported.

## Functions

*function* **SSCANF** `input-string` `control-string`

Parses formatted input text, reading characters from *input-string* and converting sequences of characters according to the *control-string* format. The *control-string* can be a string or a *compiled control-string* (see `COMPILE-CONTROL-STRING`). Returns the items converted from the *input-string* as a list. (If `control-string` is a literal string, a compiler macro will compile it.)

*function* **COMPILE-CONTROL-STRING** `control-string`

Returns a compiled function suitable for passing to `SSCANF`.

## Format

The `control-string` specifies a *picture* of the input to matched. Characters (other than '`%`') are matched one-for-one, excpet that (any amount of) whitespace in the `control-string` will match any amount of whitespace in the `input-string`. The following description is valid for the **STANDARD-CONVERTER**. (See below at **Configuring** for more information on what this means.)

*Conversion specifiers* begin with a `%`, and are followed by (in order):

* An optional *assignment suppression flag* (the character '`*`'), indicating that the matched object shoud not be returned in the list of results.

* An optional maximum *field width* (one or more decimal digits)

* An optional *length modifier* (one of `hh`, `h`, `l`, `ll`, `j`, `z`, `t` or `L`)

* A required *conversion specifier* (one of `a`, `c`, `d`, `e`, `f`, `g`, `i`, `o`, `s`, `u`, `x` and `%`)

Unlike C99, the STANDARD-CONVERTER does not accept `p` (pointer) or `n` conversions.

For the meaning of the *conversion specifiers* please refer to the closest man page or language specification.

## Example

The format of `/proc/net/unix` is defined in the `unix_seq_show()` function of the Linux kernel as:

    "%pK: %08X %08X %08X %04X %02X %5lu"

followed by a space and a path string (where `K` is a special kernel format we can treat as `x`).

Therefore we can scan lines with the *control-string*:

    "%x: %8x %8x %8x %4x %2x %5lu %s"

For example:

```lisp
SCANFCL> (sscanf "000000004713b902: 00000002 00000000 00010000 0005 01 19462 /run/WSL/8_interop" 
                 "%x: %8x %8x %8x %4x %2x %5lu %s")

(1192474882 2 0 65536 5 1 19462 "/run/WSL/8_interop")
```

## Configuring

**scanfcl** provides some measure of control over how *control-strings* are processed. Configuration is accomplished by specialising generic functions on a *converter* class with an instanct bound to `*CONVERTER*`. By default this is an instance of `STANDARD-CONVERTER`.

*special variable* **\*CONVERTER\***

Bound to an instance of a class used to specialise generic functions of the configuration protocol. By default, an instance of `STANDARD-CONVERTER`.

*class* **STANDARD-CONVERTER**

The class upon which the default behaviour of **scanfcl** is specialised.

*generic function* **COLLECT-LENGTH-MODIFIER** `converter` `control-string` `control-string-index`

Return two values: a representation of the *length modifier* (if any) in `control-string` starting at `control-string-index`; and the updated value of `control-string-index`.

*generic function* **COLLECT-CONVERSION-SPECIFIER* `converter` `control-string` `control-string-index`

Return two values: a representation of the *conversion specifier* in `control-string` starting at `control-string-index`; and the updated value of `control-string-index`.

*generic function* **COLLECT-SCANSET** `converter` `control-string` `control-string-index`

Return two values: a two-item list of a *scanset* (suitable for consumption by `MAKE-CONVERSION-SCANNER` as part of a *conversion-specifier*) and a boolean indicating whether the scanset is negated; and the updated value of `control-string-index`.

*generic function* **COLLECT-FIELD-WIDTH** `converter` `control-string` `control-string-index`

Return two values: the *field width* (if any) specified in `control-string` starting at `control-string-index`; and the udpated value of `control-string-index`.

*generic function* **MAKE-CONVERSION-SCANNER** `converter` `conversion-specifier` `suppressp` `field-width` `length-modifers`

Return a scanner, a function of no arguments returning an appropriate value from `*string*` given the arguments.