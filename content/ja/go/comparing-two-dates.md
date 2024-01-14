---
title:                "Go: 日付を比較する"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ比較するのか

日付を比較することに関心があるかもしれません。例えば、プロジェクトの締め切りを適切に管理するために、今日の日付と締め切り日付を比較する必要があるかもしれません。または、アプリケーションで特定の期間のデータを取得する際に、開始日と終了日を比較する必要があるかもしれません。今回は、Go言語を使用して日付を比較する方法を紹介します。

## 方法

まずは、比較する日付を準備しましょう。ここでは、今日の日付を取得する方法を紹介します。```Go
today := time.Now()
```
次に、比較する日付を別の変数に格納します。
```Go
deadline := time.Date(2020, time.April, 30, 23, 59, 59, 0, time.Local)
```
これで、今日の日付と締め切り日付を比較する準備が整いました。次のコードを使用して、どちらが後の日付なのかを確認できます。
```Go
if today.After(deadline) {
	fmt.Println("今日は締め切りを過ぎています。")
} else if today.Before(deadline) {
	fmt.Println("締め切りまで残り時間があります。")
} else {
	fmt.Println("今日が締め切りです。")
}
```
出力結果は以下のようになります。
```
締め切りまで残り時間があります。
```

## ディープダイブ

日付の比較には、```After()```や```Before()```以外にも、より詳細な比較方法があります。例えば、2つの日付の差を計算するための```Sub()```や、年、月、日などの個別の要素を取得するための```Year()```や```Month()```、```Day()```などのメソッドがあります。これらのメソッドを使用することで、さまざまな日付の比較を行うことができます。

## See Also

- [Go言語ドキュメント: 時間と日付](https://golang.org/pkg/time/)
- [Go by Example: 時間と日付](https://gobyexample.com/time)
- [Go World: 日付を比較する方法](https://www.goworld.jp/library/171/programming/golang-compare-date-time.html)