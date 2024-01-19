---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

日付の解析とは、形式化された文字列から日付のデータを抽出することです。プログラマーがこれを行う一番の理由は、適切なデータ構造にデータを変換し、日付に関連する他の操作（計算、比較など）を促進するためです。

## 方法は：

この例では、Go言語を使用して、ISO 8601の日付文字列(YYYY-MM-DD形式)を解析します。

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	const dateForm = "2006-01-02"
	str := "2022-03-12"

	t, err := time.Parse(dateForm, str)
	if err != nil {
		fmt.Println("パースエラー", err)
		return
	}
	fmt.Println(t)
}
```

このプログラムを実行すると以下のような出力が得られます。

```Go
2022-03-12 00:00:00 +0000 UTC
```

## ディープダイブ：

Go言語の`time`パッケージでは、日付を文字列から解析するために`Parse`関数を提供しています。 

1. **歴史的背景**:  Go言語の初期バージョンからこの関数は存在しており、その使いやすさと強力さのため広く使われています。

2. **代替手段**: `time.Parse`関数以外にも、リーダブルな相対的な時間表現（"3 days ago"など）を解析するためのサードパーティライブラリが多数存在します。これらは特定のニーズに応じて使用することができます。

3. **実装の詳細**: `time.Parse`は、指定のフォーマットに従って文字列を解析し、結果を`time.Time`型として返します。エラーが発生した場合、エラー情報も返します。 

## 参考資料：

以下ではParsing(layout, value string)やその他の関数の使用方法を詳しく説明しています。

1. Go公式ドキュメンテーション - [timeパッケージ](https://golang.org/pkg/time/)
2. Goのdate and timeのフォーマットについて解説した [Go言語のブログ記事](https://yourbasic.org/golang/format-parse-string-time-date-example/)
3. Goで時間を扱うための [便利なライブラリのリスト](https://awesome-go.com/#date-and-time)