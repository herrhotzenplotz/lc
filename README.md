# Untyped lambda calculus interpreter

Just something I did in my spare time.

## Quick start

```bash
$ git clone git@github.com:herrhotzenplotz/lc
$ cd lc/
$ stack build
$ stack run
```

## Examples to try

+ `(\x.x \y.y)`
+ `((\x.\y.(x y) \f.f) \r.r)`

## Contributions

If you want to submit pull requests or file issues, please feel free
to do that. I'll address them as soon as I can.

## License

The lc is released under a three clause BSD license. Please see the
LICENSE file.
