---
title:                "Go: 「2つの日付を比較する」"
simple_title:         "「2つの日付を比較する」"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Why (なぜ):

プログラムを開発する際に、日付の比較は非常に重要な要素です。例えば、予約システムやイベント管理アプリなど、特定の日付を基準にデータを整理する必要があります。Go言語では、日付の比較を簡単に行うことができます。

# How To (方法):

日付を比較するには、比較したい二つの日付を `time.Time` という型で定義します。その後、`After()`、`Before()`、または`Equal()`メソッドを使用して、日付を比較することができます。

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	date1 := time.Date(2021, time.March, 24, 0, 0, 0, 0, time.UTC)
	date2 := time.Date(2021, time.March, 25, 0, 0, 0, 0, time.UTC)

	// date1 が date2 よりも前の日付かどうかを判定
	fmt.Println(date1.Before(date2)) // 出力結果：true

	// date1 が date2 よりも後の日付かどうかを判定
	fmt.Println(date1.After(date2)) // 出力結果：false

	// date1 と date2 が同じ日付かどうかを判定
	fmt.Println(date1.Equal(date2)) // 出力結果：false
}
```

出力結果では、日付の比較結果が`true`か`false`で表示されます。また、`time.Time`型ではなく、文字列で日付を比較することも可能です。

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	date1 := "2021-03-24"
	date2 := "2021-03-25"

	if date1 < date2 { // 文字列として日付を比較
		fmt.Println("date1 は date2 よりも前の日付です。")
	} else if date1 > date2 {
		fmt.Println("date1 は date2 よりも後の日付です。")
	} else {
		fmt.Println("date1 と date2 は同じ日付です。")
	}
}

```

出力結果：`date1 は date2 よりも前の日付です。`

# Deep Dive (詳細):

Go言語では、日付を比較するために `time.Time` 型が使用されます。この型は日付と時間を管理するために必要な様々なメソッドを持っており、日付を比較する際に非常に便利です。

また、`time.Time`型は、`unix`時間というエポックタイムスタンプで表される日時のタイプです。エポックタイムスタンプは、UTC 1970年1月1日から数えた秒数で表されます。このような表現方法を使うことで、日付をより精確に比較することができます。

# See Also (関連リンク):

- [Go言語公式ドキュメント - 日付と時刻](https://golang.org/pkg/time/)
- [「Go 言語で日付・時刻を扱う方法【timeパッケージ】」(Qiita記事)](https://qiita.com/gofuto/items/ab5d6bb7e3684472a676)
- [「Goで日付を比較する方法」(Qiita記事)](https://qiita.com/Mahiru/items/bb3747f544a74f81d0a9)