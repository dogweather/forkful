---
title:                "Go: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# なぜ

日付を文字列に変換することが重要なのか？日付を演算したり、フォーマットしたりする必要がある場合、日付を文字列に変換して扱うことがよくあります。Go言語では、特にタイムゾーンやロケールの設定が重要な場合に便利です。

# 方法

日付を文字列に変換する方法は、Go言語で非常に簡単です。以下の例をご覧ください。

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // 現在の日付を文字列に変換する
    currentDate := time.Now().Format("2006年01月02日") // Output: 2020年10月01日
    fmt.Println(currentDate)

    // 指定した日付を文字列に変換する
    date := time.Date(2020, time.October, 01, 0, 0, 0, 0, time.UTC)
    convertedDate := date.Format("2006年01月02日") // Output: 2020年10月01日
    fmt.Println(convertedDate)
}
```

以上のコードを実行すると、現在の日付と指定した日付をそれぞれ文字列に変換していることがわかります。`time.Now()`を使用することで、現在の日付を取得し、`.Format()`を使用することで指定したフォーマットに従って文字列に変換することができます。もちろん、`time.Date()`を使用して任意の日付を指定することも可能です。

# ディープダイブ

日付を文字列に変換する際には、フォーマットによって異なる表現が得られます。Go言語で使用できるフォーマットには、以下のようなものがあります。

- 2006-01-02: 数値表記の年、月、日
- Jan 02, 2006: 短縮表記の月、日、年 (例: Oct 01, 2020)
- January 02, 2006: 冗長な表記の月、日、年 (例: October 01, 2020)
- Mon, Jan 02, 2006: 短縮表記の曜日、月、日 (例: Thu, Oct 01, 2020)
- Monday, Jan 02, 2006: 冗長な曜日、月、日 (例: Thursday, October 01, 2020)

また、`time`パッケージには、タイムゾーンやロケールを指定することもできます。これにより、日付を特定の地域のフォーマットに従って変換することができます。詳細はドキュメンテーションをご覧ください。

# 参考リンク

- [Go言語公式ドキュメント: `time`パッケージ](https://golang.org/pkg/time/)
- [Go言語でまったく新しい日付と時刻を手に入れる](https://www.calhoun.io/creating-random-uuid-in-go/)
- [Go言語の時間表記 "2006年" から意味をつけてみる](https://qiita.com/Sekky0905/items/869a119f915b2cc5bfc6)
- [Go言語での日付と時刻の操作方法](https://codezine.jp/article/detail/8946)
- [Go言語での日付と時刻のフォーマット指定](https://qiita.com/nanamen/items/08e43dc6aef7c916043b)

# 関連リンク