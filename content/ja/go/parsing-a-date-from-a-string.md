---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:36:34.047813-07:00
simple_title:         "文字列から日付を解析する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列から日付を解析するのは、テキスト形式の日付をプログラムで扱える日付型に変換することです。ファイル、API、ユーザーの入力から日付を読み取り利用するために行います。

## How to: (やり方)
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	layout := "2006-01-02 15:04:05" // the reference time format
	input := "2023-03-14 09:26:00"
	parsedTime, err := time.Parse(layout, input)
	if err != nil {
		fmt.Println("Error parsing time:", err)
		return
	}
	fmt.Println("Parsed time:", parsedTime)
}

// 出力: Parsed time: 2023-03-14 09:26:00 +0000 UTC
```

## Deep Dive (深堀り)
Go言語では、`time`パッケージが日付の解析を担います。"2006-01-02 15:04:05"はGoのユニークな参照日時形式で、この形式を利用してパターンを作ります。`Parse`関数は、文字列とレイアウトパターンを使って日付を解析します。他の言語では異なる方法を採用しているかもしれませんが、Goのアプローチは一貫しています。`time.RFC3339`のような定義済みのレイアウトを使うこともできます。また、`ParseInLocation`を使えばタイムゾーンを考慮した解析も可能です。

## See Also (関連情報)
- Goの公式ドキュメント: [timeパッケージ](https://pkg.go.dev/time)
- タイムゾーンに関する解析: [ParseInLocation](https://pkg.go.dev/time#ParseInLocation)
- Goの日付と時間のフォーマットについて詳しく学ぶ: [Go by Example: Time Formatting / Parsing](https://gobyexample.com/time-formatting-parsing)
