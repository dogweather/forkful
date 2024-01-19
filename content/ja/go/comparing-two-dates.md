---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何となぜ？
日付の比較とは一般に2つの日付が等しいか、あるいはどちらが新しいかを決定するプログラミングの操作手段です。それは、日付のデータを整理、フィルタリング、またはソートするために何度も使われています。

## どうやって：
以下のコードが日付の比較を行います。Go言語には日付ライブラリが含まれており、これを使うことで日付の比較が簡単になります。

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	date1 := time.Date(2021, 10, 10, 23, 0, 0, 0, time.UTC)
	date2 := time.Date(2022, 10, 10, 23, 0, 0, 0, time.UTC)

	if date1.Before(date2) {
		fmt.Println("日付1は日付2より前です")
	} else if date1.After(date2) {
		fmt.Println("日付1は日付2より後です")
	} else {
		fmt.Println("日付1と日付2は同じです")
	}
}
```

出力:
```
日付1は日付2より前です
```

## ディープダイブ：
- 歴史的背景：初期のプログラミング言語では、日付データは基本的に文字列として扱われ、その比較は手間がかかる作業でした。しかし、現代の多くの言語（Goを含む）では、組み込みの日付/時間型が提供され、日付の比較がかなり容易になっています。
- 代替策：日付の比較には、日付と時刻を扱うための多くのサードパーティのライブラリがあります。しかし、標準の`time`パッケージは非常に強力で、大部分のニーズを満たします。
- 実装の詳細：`Before`と`After`メソッドは、UNIX時間（1970年1月1日からのナノ秒）を使用して日付を比較します。

## 参照元：
- Goの公式ドキュメンテーション：https://golang.org/pkg/time/#Time.After
- Goによる日付の操作とフォーマット：https://go.dev/play/p/UsIznpq2vwR
- スタックオーバーフローの日付比較の議論：https://stackoverflow.com/questions/12691693/dates-comparison-in-go