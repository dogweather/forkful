---
title:                "Go: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# なぜGo言語を学ぶ必要があるのか

Go言語は近年、プログラミング界隈で注目を集めている言語の一つです。特に、クラウド環境での高いパフォーマンスと簡潔な構文が評価され、多くの開発者がGo言語を取り入れています。その中でも重要な機能の一つに、文字列を小文字に変換する方法があります。本記事では、Go言語で文字列を小文字に変換する方法を紹介します。

## Go言語で文字列を小文字に変換する方法

Go言語では、文字列を小文字に変換するための組み込み関数`strings.ToLower()`を使用します。この関数は、引数として受け取った文字列を全て小文字に変換した結果を返します。

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "HELLO WORLD"
	fmt.Println(strings.ToLower(str))
}
```

このコードを実行すると、以下のような結果が得られます。

```
hello world
```

## 文字列を小文字に変換する方法の詳細

`strings.ToLower()`関数は、引数として与えられた文字列の全ての文字を、Unicodeに従って小文字に変換します。そのため、英字以外の文字も正しく小文字に変換されます。

また、この関数は元の文字列を破壊しないため、変換後の結果を新しい変数に代入する必要があります。もし元の文字列を変更したい場合は、`strings.ToLower()`ではなく`strings.ToLowerIov()`関数を使用します。

## 参考リンク

- [The Go Programming Language](https://golang.org/)
- [strings package](https://golang.org/pkg/strings/)
- [Learn Go in Y minutes](https://learnxinyminutes.com/docs/ja-jp/go-ja/)

## 関連リンク

- [Golang.jp](https://golang.jp/)
- [Awesome-Go](https://github.com/avelino/awesome-go)