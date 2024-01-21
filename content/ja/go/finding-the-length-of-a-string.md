---
title:                "文字列の長さを求める"
date:                  2024-01-20T17:47:36.733086-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の長さとは、その中にある文字の数です。プログラマーはデータを処理したり、入力のバリデーションをするために文字列の長さを求めます。

## How to: (やり方)
```Go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// ストリングの長さを見つける簡単な例
	simpleString := "こんにちは"
	fmt.Println(len(simpleString)) // 出力: 15 (バイト数としての長さ)

	// UTF-8文字列の正確な長さを見つける
	actualLength := utf8.RuneCountInString(simpleString)
	fmt.Println(actualLength) // 出力: 5 (実際の文字数)
}
```

## Deep Dive (深堀り)
`len`関数はGoで提供されており、文字列の長さをバイト単位で返します。しかし、多言語対応を考慮すると、`len`の結果は常に期待した文字数ではありません。なぜなら、UTF-8エンコーディングでは、1文字が複数のバイトで表現されることがあるからです。日本語を含む多くの言語がこの事情に該当します。

`utf8.RuneCountInString`関数は各UTF-8エンコードされた文字（ルーンと呼ばれる）を正確に数え、プログラマが期待する「文字数」を提供します。

歴史的に、プログラミングはASCII文字セットを使用してシンプルな英語のテキスト処理を行っていました。しかし、グローバルな使用の増加とともに、多様な文字セットに対応する必要が生じ、UTF-8が広く受け入れられるようになりました。

代替方法としては、`range`を使ってルーンを反復処理することができます。これも正しい文字数を計算する一つの手法です。

```Go
length := 0
for range simpleString {
	length++
}
```

## See Also (関連情報)
- Goのドキュメンテーション: [Strings, bytes, runes and characters in Go](https://blog.golang.org/strings)
- Unicodeについてのより詳しい情報: [The Unicode Consortium](https://home.unicode.org/)
- Goプログラミング言語に关する情報: [A Tour of Go](https://tour.golang.org/welcome/1)
- Goの`unicode/utf8`パッケージのドキュメンテーション: [Package utf8](https://pkg.go.dev/unicode/utf8)