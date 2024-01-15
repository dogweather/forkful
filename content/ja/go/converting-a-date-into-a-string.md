---
title:                "日付を文字列に変換する"
html_title:           "Go: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換する理由は多々あります。例えば、データベースやファイル保存など、データの形式を統一する必要がある場合や、ユーザーインターフェースに表示する際に、日付を自由なフォーマットで表現する必要がある場合などです。

## 変換方法

日付を文字列に変換する方法はGo言語では簡単です。まずは、日付を表す変数を用意します。例えば、`time.Now()`を使用して現在の日付を取得し、`time.Time`型の変数`date`に代入するとします。

```
Go
date := time.Now()
```

次に、`fmt`パッケージの`Printf`関数を使用して、日付を指定したフォーマットの文字列に変換します。例えば、`02-Jan-2006`というフォーマットを使用すると、日付を`05-Feb-2021`のような文字列に変換することができます。

```
Go
fmt.Printf("%02d-%s-%d\n", date.Day(), date.Month().String()[:3], date.Year())
// output: 05-Feb-2021
```

## 深堀り

Go言語では、`time`パッケージを使用して日付や時間を扱うことができます。`time`パッケージは、日付や時間の表現に必要な型やメソッドを提供しています。また、`fmt`パッケージの`Printf`関数は、指定したフォーマットに従ってデータを表示するため、日付を文字列に変換する際に便利です。

しかし、日付の文字列をパースしたり、タイムゾーンを考慮したりと、さらに詳細な操作を行う場合は、`time`パッケージのドキュメントを参照する必要があります。日付と時間の扱いに関する詳しい情報が記載されていますので、ぜひ活用してください。

## 参考リンク

- [timeパッケージドキュメント](https://pkg.go.dev/time)
- [fmtパッケージドキュメント](https://pkg.go.dev/fmt)