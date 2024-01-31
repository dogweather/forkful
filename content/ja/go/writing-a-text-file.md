---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキストファイルを書くことは、データを永続的に保存します。プログラマは設定、ログ、データ交換のためにこれを行います。

## How to: (方法)
```Go
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Create("example.txt")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer file.Close()

	writer := bufio.NewWriter(file)
	_, err = writer.WriteString("こんにちは、世界\n")
	if err != nil {
		fmt.Println(err)
		return
	}

	err = writer.Flush()
	if err != nil {
		fmt.Println(err)
		return
	}
}
```
出力: `example.txt` ファイルに "こんにちは、世界" が書かれます。

## Deep Dive (深く掘り下げて)
過去、テキストファイルは主要なデータ記録手段でした。現在は、JSON, XMLなどの代替フォーマットがありますが、簡易性と可読性からテキストファイルが好まれることも多いです。Go言語では、`os`と`bufio`パッケージで効率的なファイルライティングができます。エラー処理と`defer`ステートメントが重要です。

## See Also (関連情報)
- Go by Example: Writing Files: https://gobyexample.com/writing-files
- Go Doc: os package: https://pkg.go.dev/os
- Go Doc: bufio package: https://pkg.go.dev/bufio
