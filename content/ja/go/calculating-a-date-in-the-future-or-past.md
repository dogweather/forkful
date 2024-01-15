---
title:                "将来または過去の日付を計算する"
html_title:           "Go: 将来または過去の日付を計算する"
simple_title:         "将来または過去の日付を計算する"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を未来や過去の日付に計算することの重要性は、日常生活において多くの場面で使用されるからです。例えば、マイレージの有効期限や課題の締め切り日などの計算に便利です。

## 方法

計算を行うには、Go言語に組み込まれた "time" パッケージを使用します。以下がサンプルコードと出力例です。

```Go
// 今日の日付を取得
today := time.Now()

// 未来の日付を計算する例、30日後の日付を取得
futureDate := today.AddDate(0, 0, 30)

// 過去の日付を計算する例、1年前の日付を取得
pastDate := today.AddDate(-1, 0, 0)

// 出力
fmt.Println(futureDate.Format("2006年01月02日"))  // 30日後の日付（例：2021年05月24日）
fmt.Println(pastDate.Format("2006年01月02日"))    // 1年前の日付（例：2020年04月24日）
```

## ディープダイブ

日付の計算に使用される "AddDate()" 関数は、内部的には "Date()" 関数を使用しています。この関数は、与えられた年、月、日の数値を加算（未来の日付を計算する場合）または減算（過去の日付を計算する場合）し、新しい日付を作成します。

現在、 "time" パッケージには他にも日付や時刻を取得や変更するための関数が多数存在します。詳細な情報は公式ドキュメントを参照してください。

## 他に見る

- [Go言語公式ドキュメント - timeパッケージ](https://golang.org/pkg/time/)
- [A Tour of Go - timeパッケージ](https://go-tour-jp.appspot.com/basics/15)