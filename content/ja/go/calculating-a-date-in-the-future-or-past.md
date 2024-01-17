---
title:                "未来や過去の日付を計算する"
html_title:           "Go: 未来や過去の日付を計算する"
simple_title:         "未来や過去の日付を計算する"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何となぜ？

計算するとは、未来や過去の日付を求めることです。プログラマーがこれをするのは、アプリケーションで日付に関する様々な操作を行う必要があるからです。

## 方法：

```Go
Today := time.Now()
FutureDate := Today.AddDate(0, 1, 0)
fmt.Println(FutureDate)
```
`AddDate()`関数を使用することで、今日の日付を基準にして未来の日付を計算することができます。第一引数は年、第二引数は月、第三引数は日の増減値を指定します。このように、Goでは簡単に未来の日付を計算することができます。

## ディープダイブ：

計算することは、日付の操作が必要だった歴史的背景に根ざしたものです。また、他のプログラミング言語でも同様の機能を持つものがありますが、Goのように簡単に使えるものはありません。Goでは、`time`パッケージを使用することで、日付や時間を扱うことができます。

## 関連リンク：

- [Goプログラミング言語ドキュメント](https://golang.org/doc/)
- [AddDate()関数のドキュメント](https://golang.org/pkg/time/#Time.AddDate)
- [Goで日付を計算する方法](https://www.callicoder.com/golang-date-time-tutorial/)