---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ？

部分文字列の抽出とは特定の部分を文字列から取得することです。これはプログラマーがデータを分析しやすいように特定の情報を隔離する必要があるときにします。

## どのように：

以下はGoLANGで部分文字列を抽出する方法についての単純なソースコードです。```Go```コードブロックを用いて提供されます。

```Go
package main

import (
	"fmt"
)

func main() {
	str := "こんにちは、世界"
	fmt.Println(str[0:5])
}
```

上記のコードを実行すると次のような出力が得られます。

```Go
こんに
```

## ディープダイブ

部分文字列の抽出は文字列操作の歴史とあいまって、早くから使用されていました。Go言語では、インデックスを利用して部分文字列を抽出します。他の言語、例えばPythonではスライスを使います。両者の差異は主に実装の詳細で、GoはASCII(byteベース)とunicodeを取り扱えます。

## 参考リンク

部分文字列の抽出についてさらに知りたい方は以下を参照してください。

1. Go公式ドキュメンテーション: [Strings, bytes, runes and characters in Go](https://blog.golang.org/strings)

2. Go by Example: [String Functions](https://gobyexample.com/string-functions)

3. The Go Playground: [Substring examples](https://play.golang.org/p/j-I9BGgj2SW)

さらなる学習と努力により、部分文字列の抽出技術を磨くことができます！