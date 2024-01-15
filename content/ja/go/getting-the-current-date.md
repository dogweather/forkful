---
title:                "現在の日付の取得"
html_title:           "Go: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

現在の日付を取得する理由はさまざまあります。たとえば、タイムスタンプを使用してデータベースのエントリを作成したり、タイムゾーンに基づいてユーザーに表示される情報を制御したりする場合があります。

## ハウツー

日付を取得するには、`time.Now()`関数を使用します。以下に例を示します。

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	fmt.Println(currentTime)
}
```

このコードを実行すると、現在の日付と時刻が出力されます。`time.Now()`関数は、使用しているコンピューターの現在のローカルタイムゾーンを基準として日付を取得します。

## ディープダイブ

`time.Now()`関数は、Go言語の標準ライブラリの一部であり、非常に柔軟で使いやすいものです。この関数は、時間、分、秒、ナノ秒などの詳細な日付と時刻情報を取得することができます。また、使用するタイムゾーンを指定することもできます。

さらに、`time`パッケージには、日付と時刻を操作するためのさまざまなメソッドや関数が用意されています。これらを組み合わせて使用することで、より複雑な日付操作も可能です。

## 参考リンク

- [Go言語公式ドキュメント：timeパッケージ](https://golang.org/pkg/time/)
- [Go言語で日付と時刻を扱う方法](https://qiita.com/tenntenn/items/5766dee9cfb106fedc3b)
- [Go言語の日付と時刻の処理について理解しよう](https://kakinoki721.hatenablog.com/entry/2018/08/30/175449)