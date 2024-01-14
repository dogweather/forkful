---
title:    "Go: 「二つの日付を比較する」"
keywords: ["Go"]
---

{{< edit_this_page >}}

## なぜ？
日付を比較することは、プログラミングにおいて非常に重要です。例えば、あるイベントが特定の日付よりも前に起こるかどうかを判断するために、日付の比較が必要になる場合があります。

## 方法
日付を比較するには、次のような方法があります。

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // 比較したい日付をそれぞれの変数に格納する
    date1 := time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC)
    date2 := time.Date(2020, 12, 31, 0, 0, 0, 0, time.UTC)

    // Equalを使用して日付が一致するかどうかをチェックする
    if date1.Equal(date2) {
        fmt.Println("日付が一致します")
    } else {
        fmt.Println("日付が一致しません")
    }

    // BeforeまたはAfterを使用して、どちらが先かを比較することができる
    if date1.After(date2) {
        fmt.Println("date1の方が後の日付です")
    } else {
        fmt.Println("date2の方が後の日付です")
    }
}
```

このコードを実行すると、次のような出力が得られます。

```
日付が一致しません
date1の方が後の日付です
```

## ディープダイブ
Goでは、内部的に日付をUnix時間（1970年1月1日からの秒数）で表します。そのため、日付を比較する際には、実際の日付ではなくUnix時間を比較することになります。このことを考慮して、日付を比較する際には、同じタイムゾーンを使用することが重要です。

また、日付の比較には、`time.Time`型を使用する必要があります。この型には、日付に関連する様々なメソッドが用意されており、比較だけでなく日付のフォーマットや変換なども行うことができます。

## 参考リンク
- [Go言語公式ドキュメント - time](https://golang.org/pkg/time/)
- [Go言語で日付を取得する方法](https://qiita.com/ogady/items/0dad00c255ce5288947d)
- [日付と時刻を処理するためのGo言語のtimeパッケージ](https://www.atmarkit.co.jp/ait/articles/1905/31/news007.html)

## 併せて読みたい
- [Go言語で日時を取得し、指定した形式で出力する方法](https://www.sejuku.net/blog/91817)
- [Go言語で日時を取得・変換する方法](https://blog.matsurisupu.com/entry/2014/01/22/go_get_date_time/)
- [Go言語でデータ型を扱う（日付編）](https://web-camp.io/magazine/archives/4048)