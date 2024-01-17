---
title:                "2つの日付の比較"
html_title:           "Go: 2つの日付の比較"
simple_title:         "2つの日付の比較"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何をするのか？
日付の比較とは、二つの日付を比べて、どちらが過去の日付か、もしくは同じ日付かを判断することです。プログラマーが日付を比較する理由は、例えば顧客情報を管理するシステムで顧客の誕生日をチェックするためや、期限が過ぎたかどうかを確認するためなど、日付の情報を処理する際に必要不可欠なものだからです。

## 方法： 
```Go 
package main 

import ( 
    "fmt" 
    "time" 
) 

func main() { 
    // 二つの日付を定義 
    date1 := time.Date(2020, time.September, 21, 0, 0, 0, 0, time.UTC) 
    date2 := time.Date(2020, time.September, 22, 0, 0, 0, 0, time.UTC) 

    // 日付を比較 
    if date1.Before(date2) { 
        fmt.Println("date1はdate2よりも前の日付です") 
    } else if date1.After(date2) { 
        fmt.Println("date1はdate2よりも後の日付です") 
    } else { 
        fmt.Println("date1とdate2は同じ日付です") 
    } 
} 
```

``` 
date1はdate2よりも前の日付です 
```

## 詳しい情報：
日付の比較は、グレゴリオ暦の普及により一般的になりました。グレゴリオ暦は、1582年に制定されたもので、それまで使われていたユリウス暦よりも計算が正確です。日付の比較には、他にもUnix時間やExcelシリアル値などの方法がありますが、Goではtimeパッケージを使うことで簡単に日付の比較ができます。

## 関連情報：
- [Go Documentation: Time](https://golang.org/pkg/time/)
- [Wikipedia: Gregorian calendar](https://en.wikipedia.org/wiki/Gregorian_calendar)
- [The Julian and Gregorian Calendars](http://scienceworld.wolfram.com/astronomy/JulianandGregorianCalendars.html)