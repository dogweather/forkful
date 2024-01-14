---
title:    "Go: 未来または過去の日付を計算する"
keywords: ["Go"]
---

{{< edit_this_page >}}

## なぜ
日付を未来や過去に計算することに興味があるのか、その理由を1-2文で説明します。

Goプログラミングをしていると、時には未来や過去の日付を計算する必要が出てきます。そのような場面では、プログラムによって自動的に日付を計算することができます。例えば、レポートや予定表などを自動的に生成することができるでしょう。

## 使い方
日付を未来や過去に計算する方法を、実際のコーディング例とともに```Go ... ```コードブロックを使用して説明します。

### 日付を未来に計算する
現在の日付から特定の日数を足して、未来の日付を計算する方法をご紹介します。例えば、今日の日付から2日後の日付を計算するプログラムは以下のようになります。

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    today := time.Now()
    future := today.AddDate(0, 0, 2)
    fmt.Println("2 days from today is:", future)
}
```

上記のプログラムを実行すると、次のような結果が得られます。

```
2 days from today is: 2021-08-05 15:35:39.962724 +0900 JST m=+172883880.091250643
```

### 日付を過去に計算する
同様に、現在の日付から特定の日数を引いて、過去の日付を計算する方法は以下のようになります。

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    today := time.Now()
    past := today.AddDate(0, 0, -5)
    fmt.Println("5 days ago was:", past)
}
```

上記のプログラムを実行すると、次のような結果が得られます。

```
5 days ago was: 2021-07-28 15:35:39.962943 +0900 JST m=+172883900.889654243
```

## 深堀り
日付を未来や過去に計算する際、注意しなければならない点や、さまざまな日付計算の方法について詳しく説明します。

### 日付の演算
Go言語では、AddDate関数を使用して日付の計算を行うことができます。この関数は、日付に対して引数で指定した年数、月数、日数を加算（または減算）することで、未来や過去の日付を求めることができます。

### 月の扱い
AddDate関数では、指定した年数や月数を単純に加算（または減算）することはせず、月のオーバーフローを考慮して計算されます。つまり、例えば2月に30日を加算する場合、その月が31日までしかない場合でも、自動的に次の月へ移ります。

また、AddDate関数では負の月数を指定することもできます。例えば、-1ヶ月を引いた場合には、1ヶ月前の日付が計算されます。

## は見つける
「日付を未来や過去に計算する」に関するさらなる情報や、関連する記事を探すことができるリンクをご紹介します。

- [Go言語の日付操作](https://www.golangprograms.com/how-to-calculate-difference-between-two-date-time-in