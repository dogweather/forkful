---
title:                "文字列の補間"
date:                  2024-01-20T17:50:57.345901-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列補間（いんたぽれーしょん）とは、文字列の中に変数の内容を埋め込むことです。これはプログラムの動的な出力を生成するためや、コードの読みやすさを向上させるために行われます。

## How to: (やり方)
```Go
package main

import "fmt"

func main() {
    name := "世界"
    message := fmt.Sprintf("こんにちは、%s！", name)
    fmt.Println(message)  // 出力: こんにちは、世界！
}
```

## Deep Dive (深掘り)
Go言語では、`fmt`パッケージの`Sprintf`関数が文字列補間によく使われます。これはC言語の`sprintf`関数から来た方法で、`%s`のようなフォーマット指定子を使うことに由来します。`+`演算子で文字列を連結する方法もありますが、可読性やパフォーマンスの面で`Sprintf`が推奨されます。`Sprintf`を使うと、複雑なフォーマットも扱え、浮動小数点数や整数の形式を簡単に調整できます。

## See Also (関連情報)
- Goの公式ドキュメント: [fmt package](https://pkg.go.dev/fmt)
- 入門者向けのチュートリアル: [A Tour of Go](https://tour.golang.org/)
- 文字列操作に関する詳細: [Go by Example: String Formatting](https://gobyexample.com/string-formatting)