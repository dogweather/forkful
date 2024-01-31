---
title:                "標準エラーへの書き込み"
date:                  2024-01-19
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"

category:             "Go"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)

標準エラーに書き出すって？それはプログラムがエラーメッセージや診断情報を出力する場所です。なぜやるの？通常の出力と異なり、エラー情報をユーザーや他のシステムが識別しやすくするためだ。

## How to: (やり方)

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    _, err := os.Open("non-existing-file.txt")
    if err != nil {
        fmt.Fprintln(os.Stderr, "エラー発生：", err)
        os.Exit(1)
    }
}
```

出力例:

```
エラー発生： open non-existing-file.txt: no such file or directory
```

## Deep Dive (詳細情報)

歴史的には、UNIXシステムで標準出力（stdout）と標準エラー出力（stderr）は異なるストリームとして使われてきた。標準エラーは、ログやディスプレイへの即時の出力を意図している。Goでは、`os` パッケージにある `os.Stderr` を使ってこれを実装します。他の言語では異なる方法があるかもしれないが、コンセプトは同じだ。

## See Also (関連情報)

- Goのドキュメンテーション: [os package](https://pkg.go.dev/os)
- エラーハンドリングに関するGoブログ記事: [Error handling and Go](https://blog.golang.org/error-handling-and-go)
- Unix哲学に関する詳細: [The Art of Unix Programming](http://www.catb.org/~esr/writings/taoup/html/)
