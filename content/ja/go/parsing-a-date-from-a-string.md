---
title:                "文字列から日付を解析する"
html_title:           "Go: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何それ？なんでそれをするの？
日付を文字列から解析することは、プログラマーが文字列から日付を取得するための方法です。プログラマーは、データベースやファイルなどの文字列から日付を取得して処理したいときにこれを行います。

## 方法：
```Go
dateString := "2021-06-23"
date, err := time.Parse("2006-01-02", dateString)
if err != nil {
  fmt.Println(err)
} else {
  fmt.Println(date) // 2021-06-23 00:00:00 +0000 UTC
}
```

## 深い情報：
日付を文字列から解析する方法は、古くから存在しています。代替案として、正規表現を使用することもできますが、Go言語では標準ライブラリのtimeパッケージを使用することが推奨されています。実装の詳細については、Go言語のドキュメンテーションを参照してください。

## 関連情報：
- [Go言語のtimeパッケージのドキュメンテーション](https://golang.org/pkg/time/)
- [正規表現を使用して日付を文字列から解析する方法の参考](https://www.geeksforgeeks.org/golang-regexp-module-to-parse-a-regular-expression/)