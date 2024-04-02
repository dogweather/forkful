---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:00.173651-07:00
description: "Go\u3067\u672A\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\
  \u3092\u8A08\u7B97\u3059\u308B\u3053\u3068\u306F\u3001\u7279\u5B9A\u306E\u65E5\u4ED8\
  \u306B\u95A2\u3057\u3066\u7279\u5B9A\u306E\u30DD\u30A4\u30F3\u30C8\u3092\u6C7A\u5B9A\
  \u3059\u308B\u305F\u3081\u306B\u65E5\u4ED8\u3068\u6642\u9593\u306E\u5024\u3092\u64CD\
  \u4F5C\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u3001\
  \u7DE0\u3081\u5207\u308A\u3001\u30EA\u30DE\u30A4\u30F3\u30C0\u30FC\u3001\u3042\u308B\
  \u3044\u306F\u6642\u9593\u306E\u9032\u884C\u307E\u305F\u306F\u9000\u884C\u304C\u4E0D\
  \u53EF\u6B20\u306A\u3069\u306E\u6A5F\u80FD\u3092\u8981\u6C42\u3059\u308B\u30A2\u30D7\
  \u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\u3053\u306E\u30BF\u30B9\u30AF\u3092\u4E00\
  \u822C\u7684\u306B\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.407257-06:00'
model: gpt-4-0125-preview
summary: "Go\u3067\u672A\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\
  \u8A08\u7B97\u3059\u308B\u3053\u3068\u306F\u3001\u7279\u5B9A\u306E\u65E5\u4ED8\u306B\
  \u95A2\u3057\u3066\u7279\u5B9A\u306E\u30DD\u30A4\u30F3\u30C8\u3092\u6C7A\u5B9A\u3059\
  \u308B\u305F\u3081\u306B\u65E5\u4ED8\u3068\u6642\u9593\u306E\u5024\u3092\u64CD\u4F5C\
  \u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u3001\u7DE0\
  \u3081\u5207\u308A\u3001\u30EA\u30DE\u30A4\u30F3\u30C0\u30FC\u3001\u3042\u308B\u3044\
  \u306F\u6642\u9593\u306E\u9032\u884C\u307E\u305F\u306F\u9000\u884C\u304C\u4E0D\u53EF\
  \u6B20\u306A\u3069\u306E\u6A5F\u80FD\u3092\u8981\u6C42\u3059\u308B\u30A2\u30D7\u30EA\
  \u30B1\u30FC\u30B7\u30E7\u30F3\u3067\u3053\u306E\u30BF\u30B9\u30AF\u3092\u4E00\u822C\
  \u7684\u306B\u884C\u3044\u307E\u3059\u3002"
title: "\u300C\u672A\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u306E\u8A08\
  \u7B97\u300D"
weight: 26
---

## 何となぜ？

Goで未来または過去の日付を計算することは、特定の日付に関して特定のポイントを決定するために日付と時間の値を操作することを含みます。プログラマーは、スケジューリング、締め切り、リマインダー、あるいは時間の進行または退行が不可欠などの機能を要求するアプリケーションでこのタスクを一般的に行います。

## 方法:

Goは`time`パッケージを提供して日付と時間の操作を扱い、時間を加算または減算するための直感的なメカニズムを提供しています。ここでは、`time`パッケージを活用して未来または過去の日付を計算する方法を見ていきましょう：

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 現在の日付と時間
	now := time.Now()
	fmt.Println("Current Date and Time: ", now)

	// 未来の10日後の日付を計算
	futureDate := now.AddDate(0, 0, 10)
	fmt.Println("Date 10 Days in the Future: ", futureDate)
	
	// 過去の30日前の日付を計算
	pastDate := now.AddDate(0, 0, -30)
	fmt.Println("Date 30 Days in the Past: ", pastDate)
	
	// 現在の日付と時間に5時間30分を加算
	futureTime := now.Add(5*time.Hour + 30*time.Minute)
	fmt.Println("Future Time (5 hours and 30 minutes later): ", futureTime)
}
```

サンプル出力：
```
Current Date and Time:  2023-04-01 15:04:05.123456789 +0000 UTC
Date 10 Days in the Future:  2023-04-11 15:04:05.123456789 +0000 UTC
Date 30 Days in the Past:  2023-03-02 15:04:05.123456789 +0000 UTC
Future Time (5 hours and 30 minutes later):  2023-04-01 20:34:05.123456789 +0000 UTC
```
`AddDate`メソッドは年、月、日による日付の操作に使用され、`Add`メソッドは時間、分、秒のようなより正確な時間デルタに使用されることに注意してください。

## 深堀り

Goプログラミング言語の`time`パッケージは、Goが広く称賛される強い型の安全性と明確な構文を備えた時間操作を容易にします。その実装は、基盤となるオペレーティングシステムによって提供される時間操作機能に依存しており、効率性と正確性を保証します。歴史的に、プログラミングにおける日付と時間の取り扱いは、タイムゾーンの違い、うるう年、および夏時間の変更のために複雑さを伴ってきました。Goの`time`パッケージは、この複雑さの多くを抽象化し、開発者に時間操作のための強力なツールキットを提供します。

Goのネイティブ`time`パッケージが時間操作のニーズの広い範囲をカバーする一方で、`github.com/jinzhu/now`のような代替ライブラリは、より特定のユースケースに対する追加の便利機能と機能を提供します。これらの代替品は、ネイティブの`time`パッケージでは直接サポートされていないより複雑な日付と時間の操作ニーズに特に役立つ場合があります。

しかし、ほとんどのアプリケーションにとって、Goの組み込み時間操作機能は堅固な基礎を提供します。パフォーマンスと使いやすさのバランスを保ちながら、開発者が第三者のパッケージに手を出さずに一般的な時間関連のタスクを効率的に処理できるようにしています。
