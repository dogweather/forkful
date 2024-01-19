---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を小文字に変換するとは、全ての大文字のアルファベットを対応する小文字に変えることを指します。これを行う主な理由は、ユーザー入力の一貫性を確保し、比較操作を簡単にするためです。

## 方法:

Luaでは、文字列を小文字に変換するための機能は`string.lower()`関数によって用意されています。以下は簡単な例です。

```Lua
s = "Hello, World!"
lower_s = string.lower(s)
print(lower_s)
```

出力:
```Lua
"hello, world!"
```

これにより、文字列`s`はすべて小文字の`lower_s`に変換されます。

## ディープダイブ:

大文字を小文字に変換するというアイデアは古くから存在しており、大文字と小文字の間の比較を容易に行うために一般化されました。

Luaでは、`string.lower()`関数はCライブラリ関数`tolower()`を使用して実装されています。よって、性能に関してはあまり心配する必要はありません。

しかし、重要な点として、Lua の`string.lower()`関数はASCII文字列のみを変換します。Unicode文字列に対応する場合、lvutf8というライブラリーを使用する必要があります。

## 関連資料:

- [Lua 5.4 マニュアル](https://www.lua.org/manual/5.4/)
- [トピックについてのStack Overflow 記事](https://stackoverflow.com/questions/5866607/in-lua-how-can-i-use-string-gsub-to-change-uppercase-to-lowercase)
- [UnicodeとLua](https://www.lua.org/pil/18.html)