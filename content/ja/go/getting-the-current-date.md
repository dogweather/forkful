---
title:                "Go: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

Goプログラミングの魅力の一つに、時間や日付を簡単に取得できることがあります。日付が必要なアプリケーションを作成する際に、Go言語の日付の取得機能を活用することで、コーディングをスムーズに進めることができます。

## 方法

まず、Go言語で日付を取得するためには、標準ライブラリの"time"パッケージを使用します。具体的には、`Now()`関数を使用することで現在時刻を取得することができます。

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    currentTime := time.Now()
    fmt.Println("今日の日付は、", currentTime.Format("2006年01月02日です。"))
}
```

上記のコードを実行すると、現在の日付を`YYYY年MM月DD日`の形式で出力します。

また、特定の日付を指定して取得することもできます。例えば、2021年4月1日の日付を取得する場合は、`Date()`関数を使用します。

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    currentDate := time.Date(2021, 4, 1, 0, 0, 0, 0, time.UTC)
    fmt.Println("指定した日付は、", currentDate.Format("2006年01月02日です。"))
}
```

出力結果は以下のようになります。

```bash
指定した日付は、 2021年04月01日です。
```

## 詳細を調べる

日付の取得については、Go言語の公式ドキュメントに詳しく記載されています。`Now()`や`Date()`以外にも、曜日やタイムゾーンの取得など、様々な用途で使用できる関数が存在します。

また、日付のフォーマットについても詳しく説明されており、カスタマイズすることで自分の希望通りの形式で日付を出力することができます。

## 他にも参考になるリソース

- [Go言語公式ドキュメント](https://go.dev/)
- [A Tour of Go 日付と時刻](https://go-tour-jp.appspot.com/basics/16)
- [無闇イコール無駄！？Go における時間パッケージの活用法](https://ferret-plus.com/8566)
- [Go で日付の扱い方をマスターしよう](https://quotesmeme.com/programming/golang-date-format-date-format-golang-date-format-examples-e6b38d4e891c?gi=881ce2961aeb)