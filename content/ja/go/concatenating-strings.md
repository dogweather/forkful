---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を結合とは、2つ以上の文字列を1つにまとめることを指す。この操作は、文字列を動的に生成したり、異なるデータソースからの情報を結合するためにプログラマーによって使われます。

## 実行方法：

Goには2つの主な方法で文字列を結合することができます：`+`演算子と `fmt.Sprintf`関数。以下にその例を示します:

```Go

package main

import "fmt"

func main() {
    // +演算子を使用
    s1 := "Go" + " " + "Programming"
    fmt.Println(s1)  // 出力: Go Programming

    // fmt.Sprintfを使用
    s2 := fmt.Sprintf("%s %s", "Go", "Programming")
    fmt.Println(s2)  // 出力: Go Programming
}

```
## 詳細な解説:

Goの`+`演算子を使用した文字列の結合は、シンプルで直感的です。しかし、大量の文字列を結合する場合、`+`演算子はパフォーマンス上の問題を引き起こす可能性があります。Goの文字列は不変であり、それぞれの結合は新たなメモリを割り当てる必要があり、ガベージコレクションの負荷を増やします。

その点、`fmt.Sprintf`関数を使用して文字列を結合する方法はパフォーマンス向上に対し優れている可能性があります。これにより簡単に多数の文字列を結合することが可能になります。その一方で、この方法は型の変換に注意を要します。

歴史的に見ると、最初のGoバージョンでは`+`演算子が文字列の結合を行っていましたが、これはC言語と同様にパフォーマンスに影響を与える可能性があったため、次第に`fmt.Sprintf`関数の使用が推奨されるようになりました。

## 参考資料：

以下のリンクは、文字列の結合についてのより詳しい情報を提供します:

- Go公式ドキュメンテーション: [https://golang.org/pkg/fmt/#Sprintf](https://golang.org/pkg/fmt/#Sprintf)
- Goパフォーマンスの記事: [https://go.dev/blog/slices](https://go.dev/blog/slices)
- Go言語についてのブログ: [https://go.dev/blog/strings](https://go.dev/blog/strings)