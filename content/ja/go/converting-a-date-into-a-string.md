---
title:                "Go: 日付を文字列に変換する"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換する理由は何でしょうか？Go言語での日付の文字列への変換について説明します。

##方法

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	now := time.Now()
	fmt.Println("現在の日付:", now)

	dateString := now.Format("2006年01月02日")
	fmt.Println("文字列に変換した日付:", dateString)
}
```

出力結果:

```
現在の日付: 2021-10-17 15:12:32.994306 +0900 KST m=+0.000072301
文字列に変換した日付: 2021年10月17日
```

この例では、`time.Now()`関数で現在の日時を取得し、`Format()`関数で指定したフォーマットに従って日付を文字列に変換しています。Go言語では、`time`パッケージを利用して日付を扱うことができます。他にも、`Parse()`関数を使って文字列から日付を取得することもできます。

## ディープダイブ

Go言語では、日付を表すデータ型として`time`パッケージの`Time`型があります。これは、タイムゾーン、年月日、時分秒、ナノ秒などの情報を持っています。`Format()`関数を使うことで、この情報を指定したフォーマットに基づいて文字列に変換することができます。また、`Parse()`関数を使うことで、逆に文字列から日付型に変換することもできます。

## See Also

- [Go言語ドキュメント - timeパッケージ](https://golang.org/pkg/time/)
- [A Tour of Go - パッケージ](https://go-tour-jp.appspot.com/basics/1)