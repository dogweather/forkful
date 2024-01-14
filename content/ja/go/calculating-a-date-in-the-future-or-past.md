---
title:                "Go: 将来または過去の日付の計算."
simple_title:         "将来または過去の日付の計算."
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ
時には、未来や過去の日付を計算する必要が生じることがあります。そのような場合、Go言語を使用すると非常に簡単に実装することができます。

## 方法
Go言語を使用して未来や過去の日付を計算するには、`time` パッケージを使用します。以下のコードは、現在の日付から指定した日数を加算した未来の日付を計算する例です。

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 現在の日付を取得
	currentDate := time.Now()

	// 指定した日数後の日付を取得
	futureDate := currentDate.AddDate(0, 0, 7)

	// 結果を出力
	fmt.Println("現在の日付:", currentDate)
	fmt.Println("未来の日付:", futureDate)
}
```

以下は、上記コードの実行結果です。

```
現在の日付: 2020-10-01 13:00:00 +0900 JST
未来の日付: 2020-10-08 13:00:00 +0900 JST
```

同様に、過去の日付を計算する場合は、`AddDate` メソッドの引数に負の値を渡すことで実現できます。

```Go
// 指定した日数前の日付を取得
pastDate := currentDate.AddDate(0, 0, -7)
```

## ディープダイブ
Go言語の`time` パッケージには、未来や過去の日付を計算するためのさまざまなメソッドや機能が用意されています。詳細については、公式ドキュメントを参照してください。

https://golang.org/pkg/time/

## 参考リンク
- Go言語公式ドキュメント：https://golang.org/pkg/time/
- 日付の操作について学ぶ：https://gobyexample.com/date-time
- 日付と時刻を扱うための10のベストプラクティス：https://blog.chathurawidanage.com/2019/08/20/best-practices-for-handling-dates-and-times-in-go-lang/

See Also
## 関連リンク
- Go言語公式ドキュメント：https://golang.org/
- 日本Go言語ユーザーグループ：https://golang.jp/
- Go言語メモ帳：https://memo.yuuk.io/