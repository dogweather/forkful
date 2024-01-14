---
title:    "Go: 部分文字列を抽出する"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/go/extracting-substrings.md"
---

{{< edit_this_page >}}

# なぜサブストリングを抽出するのか

サブストリングの抽出をするメリットは、文字列の特定の部分を取得することができる点です。例えば、日付や時間の情報を含んだ文字列から特定の範囲の日付や時間だけを取り出したい場合に、サブストリングの抽出をすることで目的の情報を簡単に取得することができます。

## 抽出方法の例

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "今日は2021年8月15日です"
    date := str[3:15] // 文字列の3文字目から15文字目までを取得
    fmt.Println(date) // 出力結果: 2021年8月15日
}
```

このように、文字列から特定の範囲の部分を取り出すには、`[開始位置:終了位置]`という形式で範囲を指定し、その範囲の文字列を取り出すことができます。

## サブストリングの深堀り

サブストリングの抽出は、文字列処理の中でもよく使われる機能であり、Go言語でも簡単に実装することができます。また、さまざまなメソッドを用いることで、より柔軟なサブストリングの抽出が可能になります。例えば、`strings.Index()`を使用することで、特定の文字列の位置を取得し、その位置からサブストリングを抽出することができます。

## さらに詳しくはこちらを参照

- [Go言語ドキュメント：stringsパッケージ](https://golang.org/pkg/strings/)
- [サブストリングを取得する方法](https://www.oreilly.co.jp/community/blog/2015/10/go-substring.html)
- [サブストリングの抽出方法を学ぶ](https://blog.golang.org/strings) 

# その他の参考情報

- [Substrings in Go: Explained with Examples](https://www.freecodecamp.org/news/substrings-in-go-explained-with-examples/)
- [Understanding Substrings in Go](https://dev.to/deciduously/understanding-substrings-in-go-aen)
- [Go言語で文字列操作をする方法](https://qiita.com/Sekky0905/items/37812c1f0b22e7a8ba3b)