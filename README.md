[![Buy me a coffee][bmc-badge]][bmc-link]
[![Liberapay][lp-badge]][lp-link]
[![PayPal][ppl-badge]][ppl-link]

# dbml-mode

This major mode attempts to port all of the syntax highlighting from
https://dbdiagram.io and build upon it by providing helpers such as duplicate
checking and rendering SVGs directly in Emacs.

## How to

Clone and install manually, then simply `M-x dbml-mode`.

### Enable for file extensions

It might be useful to auto-enable the mode for certain files or patterns. One
of such methods is updating `auto-mode-alist`:

```emacs-lisp
(add-to-list 'auto-mode-alist
             '("\\.dbml\\'" . dbml-mode))
```

For every file with `.dbml` extension.

[bmc-badge]: https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee
[bmc-link]: https://www.buymeacoffee.com/peterbadida
[ppl-badge]: https://img.shields.io/badge/-paypal-grey?logo=paypal
[ppl-link]: https://paypal.me/peterbadida
[lp-badge]: https://img.shields.io/badge/-liberapay-grey?logo=liberapay
[lp-link]: https://liberapay.com/keyweeusr
