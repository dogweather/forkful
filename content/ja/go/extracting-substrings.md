---
title:                "Go: サブストリングの抽出"
simple_title:         "サブストリングの抽出"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

Go言語で文字列を抽出するのに、なぜ魅力的なのでしょうか？実際に私たちが日常生活で文字列を扱うことは多く、それらを正確に抽出できる能力は非常に重要です。例えば、ウェブ開発やテキスト処理などのアプリケーションでは、文字列抽出が欠かせません。Go言語は、高速で安全性の高いプログラミング言語のため、文字列抽出にも優れた解決策を提供してくれます。

## 技術解説

では、実際にGo言語で文字列を抽出する方法を見ていきましょう。以下のコードは、指定した開始位置と長さの文字列を抽出する方法を示しています。

```Go
package main

import "fmt"

func main() {
	str := "こんにちは、世界"
	substr := str[3:7] // "世界" を抽出
	fmt.Println(substr) // 出力: 世界
}
```

このコードでは、文字列`str`の3番目の文字から7番目の文字までを抽出しています。また、Go言語では、文字列をバイト配列として扱っているため、抽出した文字列もバイト配列として取得されます。しかし、半角文字やUTF-8の特殊文字を扱う際には注意が必要です。詳細は後述する「深堀り」のセクションで説明します。

## 深堀り

Go言語では、文字列抽出のために組み込み関数である`slice`を使用します。`slice`関数は、文字列の指定した位置から長さ分の部分を抽出して、新しい文字列を返します。しかし、Go言語の文字列はバイト配列として扱われるため、例えば日本語のように複数のバイトで表現される文字を抽出する場合、単純に位置と長さを指定しただけでは正しい結果を得ることができません。そのため、`unicode/utf8`パッケージの`RuneCountInString`関数を組み合わせて、バイト単位でなく文字単位で抽出するようにする必要があります。

```Go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	str := "こんにちは、世界"
	// 文字数をカウントして日本語の「世界」を抽出
	substr := str[utf8.RuneCountInString("こんにちは、") : utf8.RuneCountInString("こんにちは、世界")]
	fmt.Println(substr) // 出力: 世界
}
```

ここで`RuneCountInString`関数を使用しているのは、文字列のバイト単位ではなく、文字単位で位置を指定するためです。これにより、文字列を正しく抽出することができます。

## 関連リンク

- [Go言語公式ドキュメント](https://golang.org/doc/)
- [Go言語チュートリアル](https://tour.golang.org/welcome/1)
- [文字列を抽出するGo言語の組み込み関数](https://www.w3schools.in/go-language/string-functions/slice-function/)