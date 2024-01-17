---
title:                "「文字列を小文字に変換する」"
html_title:           "Go: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何 & どうして?
文字列を小文字に変換するとは何か? プログラマーがそれをする理由は?

文字列を小文字に変換するとは、文字列のすべての文字を小文字に変えることを指します。プログラマーがこれをする理由は、文字列を比較するときに大文字と小文字を区別したくないためです。例えば、"Go"と"go"は同じ文字列として扱われるため、より正確な比較が可能になります。

## 使い方:
```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "GOLANG"
    fmt.Println(strings.ToLower(str))
}
```
出力:
"golang"

## もっと詳しく:
### 歴史的背景:
文字列を小文字に変換することは、古くからプログラミング言語で行われてきました。C言語やBASICなど、昔の言語でもサポートされていました。現代の言語でも、この機能は依然重要な役割を果たしています。

### 代替方法:
文字列を小文字に変換する方法は、プログラミング言語によって異なります。Go言語では、割りと簡単に実現できますが、他の言語では複雑なコードを必要とする場合もあります。一部の言語では、特別なライブラリをインストールしなければならないこともあります。

### 実装の詳細:
Go言語で文字列を小文字に変換する方法は、stringsパッケージのToLower関数を使用することです。この関数は、文字列を小文字に変換するだけでなく、UTF-8文字にも対応しています。また、stringsパッケージにはToLowerとは異なるToTitleやToCase関数など、さまざまな文字列操作の機能が含まれています。

## 関連情報へのリンク:
- [Go言語公式ドキュメント](https://golang.org/pkg/strings/#ToLower)
- [他の言語での文字列の小文字変換方法](https://www.freecodecamp.org/news/converting-a-string-to-lower-case-in-javascript-how-to-write-performant-code-3a3aede138a7/)