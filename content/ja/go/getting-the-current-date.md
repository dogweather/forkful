---
title:                "Go: 現在の日付を取得する"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ日付を取得するのか？

プログラミングにおいて、現在の日付を取得することは重要なタスクの一つです。Go言語では、現在の日付を取得する方法が非常に簡単であり、使いやすいライブラリがたくさんあります。さあ、Go言語で現在の日付を取得する方法を学びましょう！

## Go言語で日付を取得する方法

日付を取得する方法にはいくつかの方法がありますが、今回は標準ライブラリである`time`パッケージを使用する方法を紹介します。まずは以下のように`time`パッケージをインポートします。

```
import "time"
```

次に、現在の日付を取得するには`time.Now()`関数を使用します。その後、取得した日付を表示するために`fmt`パッケージをインポートし、`Printf()`関数を使用します。

```
currentDate := time.Now()
fmt.Printf("Current date is: %v\n", currentDate)
```

もし日付の形式を変更したい場合は、`Format()`関数を使用することで指定した形式で日付を取得することができます。

```
currentDate := time.Now()
dateString := currentDate.Format("2006-01-02")
fmt.Printf("Current date is: %v\n", dateString)
```

上記のコードを実行すると、現在の日付が`2006-01-02`の形式(年-月-日)で表示されます。心配しないでください、この形式を決めたのはGo言語の開発者ではなく、一般的な日付のフォーマットが難しすぎるという理由からです。

## 深堀り：日付を取得する方法

`time`パッケージには日付を取得するだけでなく、日付の計算や比較を行うための便利な関数があります。例えば、今日から1ヶ月後の日付を取得するには以下のように`AddDate()`関数を使用します。

```
currentDate := time.Now()
nextMonth := currentDate.AddDate(0, 1, 0) // 1年後の日付を取得
fmt.Printf("Next month is: %v\n", nextMonth)
```

また、日付の比較をする場合は`Equal()`や`Before()`、`After()`などの関数を使用することで簡単に比較ができます。

```
currentDate := time.Now()
otherDate := time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC)
fmt.Println("Is current date before 2020/01/01 ?", currentDate.Before(otherDate)) // trueを出力
```

さらに詳しい情報や他の便利な関数については、公式ドキュメントを参照してください。

## ぜひ参考にしてみてください！

今回はGo言語で現在の日付を取得する方法について紹介しました。簡単なコードで日付を取得し、さまざまな日付操作ができるので、ぜひ参考にしてみてください！

## 関連リンク

- timeパッケージのドキュメント：https://golang.org/pkg/time/
- 日付のフォーマットに関する記事：https://yourbasic.org/golang/format-parse-string-time-date-example/
- 日付比較に関する解説：https://golangcode.com/date-comparisons/