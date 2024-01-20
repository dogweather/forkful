---
title:                "文字列を大文字にする"
html_title:           "Go: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の先頭を大文字にする、これは一般的に文字列のキャピタライゼーションと呼ばれます。プログラマはコード内で認識しやすくするためや、データ整形、表示改善のためにこの操作を使用します。

## 手順：

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "go プログラム"
	capitalized := strings.Title(text)
	fmt.Println(capitalized)
}
```
実行結果:
```Go
Go プログラム
```

## ディープダイブ

文字列のキャピタライゼーションは歴史的に様々な言語の特性として存在しています。例えば、Pythonでは`capitalize()`メソッド、Javaでは`toUppercase()`メソッド等があります。Go 言語では、`strings`パッケージの`Title`関数を用いることで文字列の先頭を大文字にできます。これは、文や単語の先頭を大文字にするだけでなく、非英語のアルファベット文字に対しても利用できます。

文字列を大文字にする別の方法としては、文字列内の全ての文字を大文字にする `strings.ToUpper()` もあります。しかし、これは全ての文字が大文字になるため、特定の単語だけ先頭を大文字にしたい場合には `Title`関数を使うべきです。

## 参照：

- Go ドキュメンテーション: 文字列操作 https://pkg.go.dev/strings
- Go ドキュメンテーション: strings.Title https://pkg.go.dev/strings#Title
- Go ドキュメンテーション: strings.ToUpper https://pkg.go.dev/strings#ToUpper