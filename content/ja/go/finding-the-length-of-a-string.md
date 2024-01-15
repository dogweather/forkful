---
title:                "文字列の長さを求める"
html_title:           "Go: 文字列の長さを求める"
simple_title:         "文字列の長さを求める"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを見つけることが重要な理由は、プログラミングで文字列操作を行う際に、その長さを把握する必要があるからです。例えば、入力された文字列が指定した文字数を超える場合にエラーを表示するなど、文字列の長さを知ることでプログラムをより効率的に動作させることができます。

## 使い方

Go言語では、`len`関数を使用して文字列の長さを取得することができます。以下のようなコードを書くことで、文字列の長さが取得できます。

```Go
package main

import "fmt"

func main() {
	str := "こんにちは！"

	fmt.Println(len(str)) // Output: 6
}
```

このように、`len`関数の引数に文字列を渡すことで、その長さを取得できます。また、日本語のようにマルチバイト文字で構成される文字列でも正しく長さを取得することができます。

## ディープダイブ

Go言語における文字列の長さの取得方法は、単純なように見えても内部的には複雑な処理が行われています。Goでは、ユニコードの文字列をバイト列で表現し、それぞれのバイトの数をカウントして長さとして返す仕組みになっています。そのため、マルチバイト文字を正しく扱うためにユニコードの規格に従って実装されているという特徴があります。

## 参考

- [Go言語での文字列操作](https://golang.org/pkg/strings/)
- [Goプログラミング言語ブック](https://www.amazon.co.jp/Goプログラミング言語ブック-Alan-A-A-ドノヴァン/dp/4873118220)