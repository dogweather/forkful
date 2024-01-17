---
title:                "部分文字列を抽出する"
html_title:           "Go: 部分文字列を抽出する"
simple_title:         "部分文字列を抽出する"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何？どうして？

抽出したい文字列とは、大きな文字列から特定の部分を取り出すことを指します。プログラマーたちは、情報を特定の部分に分割する必要があるため、この作業を行います。

## 方法：

```Go
package main

import "fmt"

func main() {

    str := "こんにちは、世界！"

    // substringの抽出
    subStr := str[3:6]
    fmt.Println(subStr) // "んにち"

    // 文字列の長さ
    length := len(str)
    fmt.Println(length) // 11
}
```

## 詳細：

文字列から抽出するというアイディアは古くから存在していましたが、正確なメソッドや言語によって異なります。Go言語では、スライス記法を使用して文字列の部分を指定できます。他の言語では、インデックスを使用することができます。

代替案として、正規表現、置換（replace）や削除（delete）などの方法があります。しかし、抽出は最も一般的な方法です。また、Go言語では、実装の詳細をカスタマイズすることもできます。

## 関連リンク：

- [Go言語ドキュメント](https://golang.org/)
- [Go言語ハブ](https://golanghub.com/learn/beginners-tutorial/strings)
- [正規表現](https://www.regular-expressions.info/)