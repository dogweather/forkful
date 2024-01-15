---
title:                "「CSVを使用する」"
html_title:           "Go: 「CSVを使用する」"
simple_title:         "「CSVを使用する」"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜCSVに取り組むのか

CSVは、データを表形式で保存するための一般的なフォーマットです。そのため、多くのプログラミングやデータ処理の作業において、CSVファイルはよく使われるものです。Go言語を用いてCSVファイルを扱うことで、データ処理の作業を効率的に行うことができるようになります。

## 方法

まず、`encoding/csv`パッケージをインポートします。次に、CSVファイルを読み込んで`csv.Reader`オブジェクトを作成し、データを`Read`メソッドを用いて1行ずつ読み込みます。

```go
package main

import (
	"encoding/csv"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("data.csv")
	if err != nil {
		fmt.Println("Error:", err)
		return
	}
	defer file.Close()

	reader := csv.NewReader(file)
	for {
		row, err := reader.Read()
		if err != nil {
			fmt.Println("Error:", err)
			break
		}
		fmt.Println(row)
	}
}
```

上記の例では、`data.csv`ファイルから1行ずつ読み込んだデータをターミナルに出力しています。データを読み込む際は、必要に応じて`Parse`メソッドを用いてデータ型を変換することもできます。

## ディープダイブ

`csv.Reader`オブジェクトには、データを処理するためのさまざまなメソッドが用意されています。例えば、`ReadAll`メソッドを用いれば、CSVファイルの全データを一度に読み込むことができます。また、`csv.Writer`オブジェクトを作成することで、CSVファイルにデータを書き込むことも可能です。

さらに、`encoding/csv`パッケージには、CSVファイルのヘッダー行を処理するための`ReadHeader`メソッドや、CSVフォーマットに含まれるクオート文字をカスタマイズするための`NewReaderWithOpts`関数など、便利な機能が多数用意されています。詳細な使い方やオプションについては、公式ドキュメントを参照してください。

## 関連リンク

- [Go言語公式ドキュメント - encoding/csvパッケージ](https://golang.org/pkg/encoding/csv)
- [Effective Go - CSV](https://golang.org/doc/effective_go.html#csv)