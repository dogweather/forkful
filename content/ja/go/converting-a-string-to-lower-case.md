---
title:                "文字列を小文字に変換"
date:                  2024-01-20T17:38:22.038699-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (なぜ？とは？)
文字列を小文字に変換するとは、文字の大文字を対応する小文字に変えることです。これは、大文字・小文字を区別せずデータの比較や検索を容易にするために行われます。

## How to: (方法)
Go言語で文字列を小文字にするのは簡単です。標準ライブラリの`strings`パッケージを使います。

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    originalString := "Hello, World!"
    lowerCaseString := strings.ToLower(originalString)
    fmt.Println(lowerCaseString) // 出力: hello, world!
}
```

## Deep Dive (掘り下げ)
小文字への変換は、プログラミング言語の初期からありますが、Go言語では`strings`パッケージの`ToLower`関数を使います。それはUnicode標準を利用していて、さまざまな言語の文字に対応しています。`strings.ToLower`以外にも、自分で関数を作成するという選択肢がありますが、車輪の再発明は避けたいところです。Goでは、Unicodeに準拠した正しい小文字化を保証するために、簡単に使える`ToLower`関数が提供されています。

## See Also (関連項目)
- Go言語の公式ドキュメントで`strings`パッケージについての詳細: [https://pkg.go.dev/strings](https://pkg.go.dev/strings)
- Unicodeについてより深く知る: [https://www.unicode.org](https://www.unicode.org)
- 文字列操作のGo言語のベストプラクティス: [https://blog.golang.org/strings](https://blog.golang.org/strings)
