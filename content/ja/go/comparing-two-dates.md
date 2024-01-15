---
title:                "「二つの日付を比較する」"
html_title:           "Go: 「二つの日付を比較する」"
simple_title:         "「二つの日付を比較する」"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

あなたは日付の比較をする必要がありますか？それは、よくあることです。例えば、特定の日付に関連するデータを検索するために、データベース内の日付を比較する必要があるかもしれません。Go言語を使用することで、効率的かつ正確な日付の比較が簡単に行えます。

## How To

日付を比較するには、まず`time`パッケージをインポートします。

```Go
import "time"
```

次に、比較したい2つの日付を`time`型に変換します。例えば、以下のように現在の日付と昨日の日付を定義します。

```Go
now := time.Now()
yesterday := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.Local)
```

これで、2つの日付を比較するための準備が整いました。では、実際に比較してみましょう。

```Go
if now.After(yesterday) {
  fmt.Println("今日より前の日付です")
} else if now.Before(yesterday) {
  fmt.Println("今日より後の日付です")
} else {
  fmt.Println("今日と同じ日付です")
}
```

このように、`After`や`Before`といったメソッドを使用することで、2つの日付を比較することができます。

## Deep Dive

Go言語では、`time`パッケージには数多くの関数やメソッドがあり、詳しく調べてみることでさらに複雑な日付の比較が可能になります。例えば、`Equal`メソッドを使用することで、時刻の情報を無視して2つの日付を比較することもできます。

```Go
if time.Equal(now, yesterday) {
  fmt.Println("同じ日付です")
}
```

また、日付のフォーマットを指定して比較することもできます。`Format`メソッドを使用することで、日付を特定の形式で表現し、比較することができます。

```Go
formattedNow := now.Format("2006-01-02")
formattedYesterday := yesterday.Format("2006-01-02")
if formattedNow == formattedYesterday {
  fmt.Println("同じ日付です")
}
```

Go言語の`time`パッケージには、さまざまな機能がありますので、ぜひ試してみてください。

## See Also

- Go言語の公式ドキュメント：https://golang.org/pkg/time/
- 日付の比較について：https://www.calhoun.io/how-to-compare-go-time-objects/