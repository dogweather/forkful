---
title:                "日付を文字列に変換する"
date:                  2024-01-20T17:36:37.285591-07:00
model:                 gpt-4-1106-preview
simple_title:         "日付を文字列に変換する"

category:             "Go"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
データを文字列に変換するとは、日付オブジェクトをテキスト形式に変えることです。様々な理由で行いますが、主に人間が読める形式で表示したり、特定のフォーマットでデータを保存・送信するためです。

## How to: (やり方)
Goで日付を文字列に変換する簡単な例です。標準ライブラリの`time`パッケージを使います。

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 現在の日付と時刻を取得
	currentTime := time.Now()
	
	// 標準のレイアウトを使用して日付を文字列にフォーマット
	fmt.Println("Date and Time:", currentTime.Format("2006-01-02 15:04:05"))
	
	// 友好的なフォーマット
	fmt.Println("Friendly:", currentTime.Format("Jan 2, 2006 at 3:04pm"))
}
```

出力例:
```
Date and Time: 2023-04-01 15:04:05
Friendly: Apr 1, 2023 at 3:04pm
```

## Deep Dive (探求)
日付の文字列変換は、`time`パッケージで定義された`Time`型の`Format`メソッドを使って実現します。Goでは、`Format`メソッドに与えるパターンが特殊で、定数`time.RFC3339`を使うか、参照日時`2006-01-02 15:04:05`を使ってパターンを定義します。これはGoの開発者が選んだ「マジックナンバー」で、1から12までの数を月、日、時、分、秒に割り当てています。

過去にはsprintf関数を使用して日付を文字列変換する言語もありましたが、Goでは`Time.Format`が一般的な方法です。他にも`time`パッケージは時間のパース、加算や減算、時間帯の変換など多くの機能を提供しています。

## See Also (関連情報)
- Goの公式ドキュメント: [timeパッケージ](https://golang.org/pkg/time/)
- Go言語における日付と時刻の扱い方 – [Go by Example: Time](https://gobyexample.com/time)
- Goの時間フォーマットに関する説明 – [Goでの時刻フォーマットの仕様](https://yourbasic.org/golang/format-parse-string-time-date-example/)
