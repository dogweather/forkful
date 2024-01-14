---
title:                "Go: 文字列の長さを求める"
simple_title:         "文字列の長さを求める"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ？

文字列の長さを求めることのメリットは何でしょうか？Go言語でこの機能を使うことについて、少し考えてみましょう。

## 使い方

文字列の長さを求めるには、Go言語の `len()` 関数を使います。以下に例を示します:

```Go
package main

import "fmt"

func main() {
    str := "こんにちは"
    length := len(str)
    fmt.Println(length)
}
```

このコードを実行すると、`5` という数字が出力されます。これは、"こんにちは"という日本語の文字列の長さが5バイトであることを示しています。また、Go言語はUTF-8をサポートしているため、Unicode文字も正しく扱うことができます。

## 深層ダイブ

文字列の長さを求める際には、バイト数ではなくルーン数を使うことが大切です。バイト数ではなくルーン数を使うことで、日本語などのマルチバイト文字も正しく扱うことができます。ただし、ルーン数を求めるには一工夫必要で、Go言語では `unicode/utf8` パッケージの `RuneCountInString()` 関数を使います。以下に例を示します:

```Go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    str := "こんにちは"
    length := utf8.RuneCountInString(str)
    fmt.Println(length)
}
```

このコードを実行すると、`5` という数字が出力されることを確認できるでしょう。このように、ルーン数を使うことで文字列の長さを正しく求めることができます。

## 他の参考リンク

- [Go言語のstringsパッケージ](https://golang.org/pkg/strings/)
- [Go言語のunicode/utf8パッケージ](https://golang.org/pkg/unicode/utf8/)
- [UnicodeとUTF-8の違い](https://www.wikiwand.com/ja/Unicode)
- [日本語の文字コードについて](https://www.wikiwand.com/ja/%E6%96%87%E5%AD%97%E3%82%B3%E3%83%BC%E3%83%89)