---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なに？なぜ？ ("What & Why?")
文字列を小文字に変換するとは、大文字で書かれた文字や含まれる全ての大文字を小文字に置き換えることです。プログラマーはこれを行うことで、テキストの比較や整列など、ケースに引っかからない操作を保証します。

## 使い方 ("How to:")
Gleamの`to_lower`関数を使用して文字列を小文字に変換できます。

```gleam
import gleam/string

let sentence = "HELLO, GLEAM!"
let lower_case_sentence = string.to_lower(sentence)

assert lower_case_sentence == "hello, gleam!"
```

出力:
```gleam
"hello, gleam!"
```

## 深掘り ("Deep Dive")
文字列を小文字に変換することは古代から存在する操作で、現代のプログラミング言語の多くがこれをサポートしています。しかし、一部の言語やシステムでは、別の方法（例えば、全ての大文字を特定の小文字にマッピングすること）でこの操作を行うかもしれません。Gleamでは、体系的で統一感のある方法として、標準ライブラリの一部である`string.to_lower`関数を採用しています。

## 参照 ("See Also")
関連リソースは以下の通りです：

- 公式ドキュメンテーション: [Gleam Language](https://gleam.run/docs/)
- [string.to_lower function details](https://hexdocs.pm/gleam_stdlib/gleam/string.html#to_lower/1)
- [GitHub repository](https://github.com/gleam-lang/gleam) for Gleam project and examples.