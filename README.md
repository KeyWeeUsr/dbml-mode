# dbml-mode
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![CI][ci-badge]][ci-workflow]
[![Coverage Status][cover-badge]][cover-link]
[![Buy me a coffee][bmc-badge]][bmc-link]
[![Liberapay][lp-badge]][lp-link]
[![PayPal][ppl-badge]][ppl-link]

This major mode attempts to port all of the syntax highlighting from
https://dbdiagram.io and build upon it by providing helpers such as duplicate
checking and rendering SVGs directly in Emacs.

![sample][gif]

## How to

Install it from [Melpa](https://melpa.org/#/getting-started) or clone and
install manually, then simply `M-x dbml-mode`.

### Enable for file extensions

It might be useful to auto-enable the mode for certain files or patterns. One
of such methods is updating `auto-mode-alist`:

```emacs-lisp
(add-to-list 'auto-mode-alist
             '("\\.dbml\\'" . dbml-mode))
```

For every file with `.dbml` extension.

[melpa-badge]: http://melpa.org/packages/dbml-mode-badge.svg
[melpa-package]: http://melpa.org/#/dbml-mode
[melpa-stable-badge]: http://stable.melpa.org/packages/dbml-mode-badge.svg
[melpa-stable-package]: http://stable.melpa.org/#/dbml-mode
[bmc-badge]: https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee
[bmc-link]: https://www.buymeacoffee.com/peterbadida
[ppl-badge]: https://img.shields.io/badge/-paypal-grey?logo=paypal
[ppl-link]: https://paypal.me/peterbadida
[lp-badge]: https://img.shields.io/badge/-liberapay-grey?logo=liberapay
[lp-link]: https://liberapay.com/keyweeusr
[gif]: https://i.imgur.com/OpXUUVk.gif
[ci-badge]: https://github.com/KeyWeeUsr/dbml-mode/actions/workflows/test.yml/badge.svg
[ci-workflow]: https://github.com/KeyWeeUsr/dbml-mode/actions/workflows/test.yml
[cover-badge]: https://coveralls.io/repos/github/KeyWeeUsr/dbml-mode/badge.svg?branch=master
[cover-link]: https://coveralls.io/github/KeyWeeUsr/dbml-mode?branch=master
