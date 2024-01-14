---
title:    "Go: 「パターンに一致する文字を削除する」"
keywords: ["Go"]
---

{{< edit_this_page >}}

## なぜ

プログラミングの世界には、様々な作業がありますが、その中でも特に重要なのがパターンマッチングです。パターンマッチングとは、指定したパターンに合致する文字列を検索することを指します。そして、その中でも特によく使われるのが、特定のパターンに合致する文字を削除する作業です。なぜなら、データの整形や処理の効率化など、様々な目的で活用されるからです。

## ハウツー

まず、Go言語で特定のパターンに合致する文字を削除する方法をご紹介します。最も簡単な方法は、`strings.ReplaceAll()`関数を使うことです。例えば、`strings.ReplaceAll("Hello World", "l", "")`というコードを実行すると、"Hello World"から"l"をすべて削除した文字列が返されます。しかし、この方法では特定の文字を一括で削除することしかできません。もっと複雑なパターンに合致する文字を削除する場合は、正規表現を使うことが必要になります。正規表現は、`regexp`パッケージを使うことで実現できます。以下に、`regexp`パッケージを使ったコード例を示します。

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// 正規表現をコンパイルする
	re := regexp.MustCompile(`[A-Z]`) 
	// 文字列から正規表現にマッチする文字を削除する
	result := re.ReplaceAllString("Hello World", "")
	fmt.Println(result) // ello orld
}
```

以上のように、正規表現を使うことでより柔軟な文字の削除が可能になります。

## ディープダイブ

さらに深く掘り下げると、Go言語では正規表現にマッチする部分を置換することもできます。先ほどのコード例では、正規表現にマッチした文字を空の文字列に置換しましたが、文字列を指定した文字列に置換することもできます。例えば、`re.ReplaceAllString("Hello World", "goodbye")`とすると、`Hello World`から正規表現にマッチする文字が全て`goodbye`に置換されます。また、正規表現を使わずに文字列を一括で置換したい場合は、`strings.ReplaceAll()`関数の方がより簡単に使えるかもしれません。どちらの方法を使うかは、目的に応じて選択すると良いでしょう。

## 参考リンク

- [Go言語ドキュメント](https://golang.org/doc/) 
- [正規表現チュートリアル](https://regexone.com/)
- [Strings パッケージ](https://golang.org/pkg/strings/)
- [Regexp パッケージ](https://golang.org/pkg/regexp/)

## 他に見る

- [よく使われるパターンの正規表現](https://www.geeksforgeeks.org/regular-expression-patterns/)
- [Effictive Go (日本語訳)](https://www.baldanders.info/golang/2014/10/08/effective-go-japanese-dec-20.html)