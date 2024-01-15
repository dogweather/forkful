---
title:                "サブストリングの抽出"
html_title:           "Go: サブストリングの抽出"
simple_title:         "サブストリングの抽出"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ
Go言語を使って、サブストリング（部分文字列）を抽出する必要があるかというと、例えば文字列から特定の部分だけを取り出したい場合や、入力された文字列から特定のパターンを探したい場合に役立ちます。Go言語にはサブストリングを簡単に抽出するための便利な機能があるので、ぜひ使ってみてください。

## 方法
サブストリングを抽出するには、 `strings` パッケージ内の `Substring()` 関数を使います。例えば、文字列 `Hello World` から `World` の部分だけを抽出したい場合、以下のようにコードを書くことができます。

```Go
str := "Hello World"
sub := str[6:]
fmt.Println(sub) // Output: "World"
```

また、文字列内に特定のパターンが含まれているかどうかを判断する際にもサブストリングを使用することができます。例えば、文字列 `Welcome to the Golang World` に `Golang` という単語が含まれているかどうかを判断する場合、以下のようにコードを書くことができます。

```Go
str := "Welcome to the Golang World"
pattern := "Golang"
if strings.Contains(str, pattern) {
    fmt.Printf("%s is included in the string.", pattern) // Output: "Golang is included in the string."
}
```

## ディープダイブ
サブストリングを抽出する際には、文字列のインデックス番号を使って指定した部分を取り出すことができます。また、 `strings` パッケージには文字列内の指定したパターンの位置を検索する `Index()` 関数や、特定の文字列を置換する `Replace()` 関数など、さまざまな便利な関数が用意されています。詳しくは[公式ドキュメント](https://golang.org/pkg/strings/)を参考にしてください。

## See Also
- [Go言語チュートリアル](https://tour.golang.org/welcome/1)
- [Effective Go](https://golang.org/doc/effective_go.html)
- [Go言語で学ぶデータ構造とアルゴリズム](https://www.udemy.com/course/go-datastructures-algorithms/)