---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列に値を埋め込むこと、言い換えれば文字列補間は、文字列中の特定の部分を動的な値で置き換えるプロセスを指します。これは、コードを読みやすく、保守しやすくし、プログラムの出力を動的にカスタマイズするのに有用です。

## 使い方：
Goでは、`fmt.Sprintf`関数を使用して文字列に値を埋め込むことができます。以下に例を示します。

```Go
package main

import (
    "fmt"
)

func main() {
    name := "Taro"
    age := 25

    message := fmt.Sprintf("こんにちは %s、あなたの年齢は %d 歳です。", name, age)

    fmt.Println(message)
}
```

このプログラムは、次のような出力を生成します。

```
こんにちは Taro、あなたの年齢は 25 歳です。
```

## ディープダイブ
文字列の補間は古典的なプログラミングテクニックで、C言語の printf スタイルフォーマットが元となっています。Goはこれを `fmt.Sprintf`関数として実装しています。しかし、Goには `fmt.Printf`や `fmt.Fprintf`などの代替手段も用意されています。

Goの文字列補間では%（パーセント）記号を使用することで特殊な書式指定子を設定し、値を文字列に埋め込むことが可能です。例えば、`%s`は文字列、`%d`は整数、`%f`は浮動小数点数です。

## 参考資料
Goのドキュメンテーションは非常に豊富で、さらなる情報を見つけるのに適しています。ここでは、文字列操作に関連するいくつかのリンクを提供します。

1. 公式ドキュメンテーション：[Go言語のドキュメンテーション](https://golang.org/doc/)
2. Goプログラミング入門：[Go By Example](https://gobyexample.com/)
3. Go言語のfmtパッケージ: [fmt - The Go Programming Language](https://golang.org/pkg/fmt/)