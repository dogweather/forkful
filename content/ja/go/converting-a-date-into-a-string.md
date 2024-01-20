---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

デートを文字列に変換するとは、日付データを人間が理解できる形式の文字列（例："2022年1月1日"）に変換することです。これは、ログのタイムスタンプやユーザー向けの日付出力のためにしばしば行われます。

## どのように:

Go言語で日付を文字列に変換するには、標準ライブラリ `time` を使用します。以下にサンプルコードを示します。

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	fmt.Println(t.Format("2006-01-02"))
}
```
これを実行すると、現在の日付が "YYYY-MM-DD" 形式の文字列として出力されます。

## 深掘り:

Go言語の `time` パッケージの `Format` 関数について深く理解するために、いくつかの情報を提供します。

1. **歴史的背景**: Goの `time` パッケージは、UNIX時間（1970年以降の秒数）を基本として、UNIXエポック以降の時間を表現します。`Format` 関数は、この時間表現を文字列に変換します。

2. **代替案**: 日付を自分で文字列に変換することも可能ですが、これはエラーを引き起こす可能性があります。代わりに `Format` 関数を使用することで、一貫性と正確性を保証することができます。

3. **実装詳細**: `Format` 関数は、特定の日付（具体的には "2006-01-02 15:04:05"）を参照点として、各部分を置き換えることで日付をフォーマットします。

## 参照:

- Golang公式ドキュメンテーション（`time` パッケージ）: [https://pkg.go.dev/time@v0.0.0-20221109144857-69f9f4ec42b9#Time.Format](https://pkg.go.dev/time@v0.0.0-20221109144857-69f9f4ec42b9#Time.Format)
- Golang公式ブログ（時間について）: [https://blog.golang.org/time](https://blog.golang.org/time)
- UNIX時間についてのWikipediaの記事: [https://ja.wikipedia.org/wiki/UNIX%E6%99%82%E9%96%93](https://ja.wikipedia.org/wiki/UNIX%E6%99%82%E9%96%93)