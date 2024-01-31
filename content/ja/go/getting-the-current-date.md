---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:15:08.398501-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
Go言語で現在の日付を取得するのは、イベントのタイミングを把握したり作成日を記録したりするためです。データの時系列管理やログの生成によく使われます。

## How to (操作方法)
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 現在のローカル日付と時刻を取得
	currentTime := time.Now()
	fmt.Println("現在の日付と時刻:", currentTime)

	// より詳細な日付フォーマット
	fmt.Println("整形済みの日付:", currentTime.Format("2006-01-02 15:04:05 Monday"))
}

// 出力例:
// 現在の日付と時刻: 2023-04-07 12:30:45.918576 +0900 JST m=+0.000123456
// 整形済みの日付: 2023-04-07 12:30:45 Friday
```

## Deep Dive (掘り下げ)
Go言語の`time`パッケージは時間を扱う標準機能を提供しています。`time.Now()`はGo言語が公開された当初から存在し、直感的に現在の日付と時刻を取得するために設計されました。`Format`メソッドを使ってカスタムフォーマットが可能ですが、日付をフォーマットする際には特別なレイアウト文字列 `"2006-01-02 15:04:05"`が必要で、これはGoの発表年度である2006年を基にしています。これでデータを整形すると、読みやすい形式で時刻データを処理できます。

代替方法として、`Unix()`や`UnixNano()`関数を使用してエポックタイム（1970年1月1日からの秒数またはナノ秒数）を取得することも可能です。これらはタイムスタンプをシリアライズする際によく利用されます。

実装の詳細では、`time.Location`を指定することで、UTCや別のタイムゾーンの時間を取得することもできます。

## See Also (関連情報)
- Go言語ドキュメントの`time`パッケージ: [time package - time - pkg.go.dev](https://pkg.go.dev/time)
- Goの`time.Time`型についてのドキュメント: [time.Time - time - pkg.go.dev](https://pkg.go.dev/time#Time)
