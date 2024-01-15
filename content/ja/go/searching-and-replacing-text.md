---
title:                "テキストの検索と置換"
html_title:           "Go: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ？

テキストの検索と置換を行うことになりますか？それは、コーディングの中で頻繁に使用されるタスクであり、手間を省くために効率的に行う必要があるからです。

## ハウツー

テキストの検索と置換を行うには、Go言語の`strings`パッケージを使用します。まずは `strings.Replace()` 関数を使って、単純な置換を行ってみましょう。

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "Go言語はとても簡単です。Go言語は楽しくて面白いです。"
	newText := strings.Replace(text, "Go言語", "Golang", -1)
	fmt.Println(newText)
}
```

上記のコードを実行すると、次のような結果が得られます。

```
Golangはとても簡単です。Golangは楽しくて面白いです。
```

また、`strings.ReplaceAll()`関数を使うことで、すべてのマッチする文字列を置換することもできます。

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "Go言語はとても簡単です。Go言語は楽しくて面白いです。"
	newText := strings.ReplaceAll(text, "Go言語", "Golang")
	fmt.Println(newText)
}
```

上記のコードを実行すると、同じ結果が得られますが、`-1`の代わりに` strings.ReplaceAll()`では引数を省略することができます。

## ディープダイブ

今回使用した`strings.Replace()`と`strings.ReplaceAll()`は、非常に基本的な機能を備えています。しかし、`strings`パッケージにはさらに多くの関数があり、より柔軟な検索と置換が可能です。

例えば、`strings.Replace()`には第四引数として、置換する回数を指定するオプションがあります。

```Go
newText := strings.Replace(text, "Go言語", "Golang", 1)
```

上記のようにすることで、最初のマッチした箇所のみを置換することができます。

また、`strings.ReplaceAll()`は、内部で正規表現を使用しているため、正規表現を用いた高度な検索と置換も可能です。

```Go
newText := strings.ReplaceAll(text, "[a-z]+言語", "Golang")
```

上記のように書くことで、文字列の中で先頭が小文字の文字列のみを検索し、`Golang`に置換することができます。

## さらに学ぶ

- [Go言語公式ドキュメント - stringsパッケージ](https://golang.org/pkg/strings/)
- [A Tour of Go - stringsのセクション](https://go-tour-jp.appspot.com/basics/12)

## 参考リンク

- [Replace and ReplaceAll functions in the strings package - Programming.Guide](https://programming.guide/go/replace-replaceall-strings.html)
- [Golang: replace substring with another in a string](https://stackoverflow.com/a/33860055)