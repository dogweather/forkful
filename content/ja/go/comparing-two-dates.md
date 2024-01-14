---
title:    "Go: 日付の比較"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ
Go言語において、日付を比較することは重要です。例えば、日付を使用したソートや、期限を設定した処理などにおいて、正しい日付の比較が必要となります。

## 方法
Go言語には、日付を比較するための便利なパッケージがあります。以下のコードを使用することで、簡単に日付を比較することができます。

```Go
package main
import (
    "fmt"
    "time"
)
func main() {
    t1 := time.Date(2020, time.March, 15, 0, 0, 0, 0, time.UTC)
    t2 := time.Date(2019, time.March, 20, 0, 0, 0, 0, time.UTC)

    if t1.After(t2) {
        fmt.Println("t1はt2よりも後の日付です")
    } else {
        fmt.Println("t1はt2よりも前の日付です")
    }

    if t1.Equal(t2) {
        fmt.Println("t1とt2は同じ日付です")
    } else {
        fmt.Println("t1とt2は異なる日付です")
    }
}
```
出力: 
```
t1はt2よりも後の日付です
t1とt2は異なる日付です
```
このように、`time`パッケージの`After`や`Equal`メソッドを使用することで、簡単に日付を比較することができます。

## ディープダイブ
Go言語には、日付を比較する際に使える便利な関数やインターフェースがあります。例えば、`Before`や`Between`などのメソッドを使うことで、より柔軟な日付の比較が可能となります。また、Go言語は他の言語とは違い、日付同士を直接比較することができるので、よりシンプルなコードを書くことができます。

## ご参考
[timeパッケージドキュメント](https://golang.org/pkg/time/) 
[Go言語で日付の比較をする方法](https://geeksforgeeks.org/how-to-compare-date-in-golang/)