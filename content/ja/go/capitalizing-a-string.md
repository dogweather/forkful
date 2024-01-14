---
title:    "Go: 文字列の大文字化"
keywords: ["Go"]
---

{{< edit_this_page >}}

## なぜ

なぜ文字列を大文字に変換するのか？実際のプログラミングの中で、文字列を大文字で表示したい場合があります。たとえば、人の名前を表示する際には通常大文字で始まるようにしたいでしょう。

## やり方

文字列を大文字に変換するには、`strings`パッケージの`ToUpper`関数を使用します。以下のようなGoコードを使ってみましょう。

```Go
package main

import (
  "fmt"
  "strings"
)

func main() {
  name := "john"
  fmt.Println(strings.ToUpper(name))
}
```

このコードは、変数`name`の値を大文字に変換し、`JOHN`という出力を返します。

## ディープダイブ

しかし、実際には文字列の大文字変換はもっと複雑なプロセスです。例えば、日本語の文字列を大文字にする場合、どのように扱えばいいのでしょうか？また、文字列の長さやアルファベット以外の文字についても考慮する必要があります。

## 関連リンク

- [Strings Package - GoDoc](https://golang.org/pkg/strings/)
- [How to Use Strings in Go - Tutorial by Better Programming](https://betterprogramming.pub/strings-in-golang-66251418ae42)
- [Working with UTF-8 in Go - Tutorial by Medium](https://medium.com/go-walkthrough/working-with-utf-8-in-go-5e3f0effbe49)

## 参考文献: [Markdownで記事を書く時に使う基本的な記法](https://qiita.com/tbpgr/items/989c6badefff69377da7)