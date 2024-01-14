---
title:    "Go: 日付を文字列に変換する"
keywords: ["Go"]
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換する理由は何でしょうか？Goプログラミングの世界では、日付を文字列に変換することが多々あります。例えば、データをデータベースに保存する際や、ファイル名として使う際などに利用されます。

## 方法
日付を文字列に変換する方法を説明します。まずは`time`パッケージをインポートしましょう。

```Go
import "time"
```

次に、`time.Now()`を使って現在時刻の`time.Time`型の値を取得します。この値を`Format()`メソッドで指定したフォーマットに変換することで、日付を文字列に変換できます。例えば、`yyyy年MM月dd日`の形式に変換するには、以下のようにします。

```Go
t := time.Now()
date := t.Format("2006年01月02日")
```

`Format()`メソッドでは、特定の日付を表す文字列を指定します。例えば、`2006年01月02日`はGo言語の生まれた日付であり、そのまま書くと`2006年01月02日`という日付に変換されます。

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	date := t.Format("2006年01月02日")
	fmt.Println(date)
}
```

実行すると、現在の日付が`yyyy年MM月dd日`の形式で表示されます。例えば、2021年1月1日ならば`2021年01月01日`という文字列に変換されます。

```
2021年01月01日
```

## ディープダイブ
さらに深く日付を文字列に変換するためには、`time`パッケージの`Parse()`メソッドを使う方法があります。このメソッドでは、指定した形式に合わせた文字列を解析して日付を取得します。例えば、`2006年01月02日`という形式の文字列を`yyyy/MM/dd`の形式の日付に変換するには、以下のようにします。

```Go
dateStr := "2021/01/01"
date, _ := time.Parse("2006年01月02日", dateStr)
fmt.Println(date.Format("yyyy/MM/dd"))
```

実行すると、`2021/01/01`という日付が表示されます。

```
2021/01/01
```

## 参考リンク
- [Go言語の日付操作まとめ](https://qiita.com/tenntenn/items/7b269b046d7c1e3aa5f4)
- [timeパッケージドキュメント](https://golang.org/pkg/time/)
- [Go By Example: Time Formatting / Parsing](https://gobyexample.com/time-formatting-parsing)