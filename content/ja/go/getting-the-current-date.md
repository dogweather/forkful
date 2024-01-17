---
title:                "「現在の日付を取得する」"
html_title:           "Go: 「現在の日付を取得する」"
simple_title:         "「現在の日付を取得する」"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

今日の日付を取得するには、Go言語の現在のバージョンを使用します。プログラマーは、アプリケーションの実行時間やデータのタイムスタンプなど、さまざまな目的で現在の日付を取得します。

## 何？ なぜ？

現在の日付とは、今日の日付を意味します。プログラマーは、必要に応じてアプリケーション内で現在の日付を使用します。例えば、データベースに保存するデータのタイムスタンプを更新するために使用することができます。

## 方法：

まず、現在の日付を取得するには、timeパッケージをインポートする必要があります。次に、time.Now()メソッドを使用して、現在の日付を取得します。

```Go
import "time"

currentDate := time.Now()
fmt.Println(currentDate)
```

出力：

```Go
2021-10-11 14:30:00.382389 +0000 UTC m=+0.000256676
```

また、日付のフォーマットを指定することもできます。

```Go
currentDate := time.Now()
formattedDate := currentDate.Format("01-02-2006")

fmt.Println(formattedDate)
```

出力：

```Go
10-11-2021
```

## 詳細を深く掘り下げる：

Go言語では、日付と時刻を表すためにtimeパッケージが使用されています。このパッケージには、現在の日付や時刻を取得するためのさまざまなメソッドが用意されています。例えば、Unix時間やタイムゾーンを指定するには、time.Unix()やtime.LoadLocation()メソッドを使用することができます。

また、Go言語以外でも、現在の日付を取得するためのさまざまな方法があります。例えば、Pythonではdatetimeモジュールを使用することができます。

具体的な実装の詳細については、公式ドキュメントを参照することをおすすめします。

## 関連リンク：

- [Go言語のtimeパッケージドキュメント](https://golang.org/pkg/time/)
- [Pythonのdatetimeモジュールドキュメント](https://docs.python.org/3/library/datetime.html)