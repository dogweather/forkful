---
title:    "Go: 現在の日付を取得する"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

Go言語で現在の日付を取得する必要があるのでしょうか？Go言語のプログラミングにおいて、現在の日付を取得することはよくあることです。日付を取得することで、アプリケーションやプログラムをより柔軟に動かすことができます。例えば、特定の日付に基づいて処理を行う必要がある場合や、タイムスタンプを生成する必要がある場合などがあります。

## 方法

Go言語では、timeパッケージを使って現在の日付を取得することができます。以下のコードを参考にしてください。

```
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	fmt.Println(currentTime.Format("2006/01/02"))
}
```

上記のコードでは、`time.Now()`を使って現在の日付を取得し、`Format()`を使って表示形式を指定しています。`2006/01/02`の部分は、Go言語において日付を表す特殊な形式です。このようにすることで、年、月、日を正しく表示することができます。

上記のコードを実行すると、次のような出力結果が得られます。

```
2021/08/25
```

## ディープダイブ

`time.Now()`では、現在のローカルタイムを取得することができます。しかし、タイムゾーンを指定することもできます。例えば、日本のタイムゾーンでの現在の日付を取得するには、`time.Now().In(JST)`のようにすることができます。JSTは、`time.Location`型の変数であり、予め定義されています。

また、`time.Now()`は、UTC（協定世界時）を基準とした日付を返します。そのため、夏時間や冬時間の影響を受けません。もし、現在の日付をローカルタイムではなくUTCベースで取得したい場合は、`time.Now().UTC()`を使うことができます。

## 相関項目

- [Go言語公式ドキュメント - timeパッケージ](https://golang.org/pkg/time/)
- [Go言語公式ドキュメント - timeパッケージの例](https://golang.org/pkg/time/#example_Now)
- [Go言語での日付処理のベストプラクティス](https://blog.learngoprogramming.com/go-time-time-format-time-parse-time-zone-timezone-conversions-4edfedef5ec5)