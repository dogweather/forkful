---
title:                "文字列の大文字化"
html_title:           "Gleam: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何? なぜ?
文字列を大文字にすることは、プログラマーたちがよくやることです。プログラマーは、文字列を大文字にすることで、コードをより読みやすくし、意図を明確にすることができます。

## 手順:
`Gleam ...`のコードブロック内にコーディングの例とサンプルの出力を示します。

```
Gleam import gleam/string
let name = "gleam"
gleam/string.to_upper(name)
```
上記のコードを実行すると、`"GLEAM"`という文字列が出力されます。

## 深く掘り下げる
文字列を大文字にする処理は、プログラミングの世界ではよく用いられるものです。歴史的な文脈からいえば、これは古くからある基本的な処理であり、今日でもよく使われています。代替手段としては、Pythonの`upper()`メソッドなどがありますが、Gleamではこのような簡単な処理でも型安全やパターンマッチングなどの機能を提供するため、大文字にする処理もより安全かつエレガントに記述することができます。

## 関連情報
便利な文字列操作を学ぶために、以下のリンクを参考にしてみてください。
- [Gleam公式ドキュメント](https://gleam.run/)
- [文字列操作の基本機能](https://medium.com/@alkemann/gleam-trickery-for-cli-tools-or-general-purpose-functions-aa5d94f2d3b0)
- [Pythonの文字列操作方法](https://realpython.com/python-strings/)