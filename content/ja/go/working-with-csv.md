---
title:                "CSVファイルの操作"
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV (Comma-Separated Values)ファイルは、データ保管と転送に使われています。プログラマはCSVでの作業が速くて、多様なシステムと簡単に互換性があるからです。

## How to:

```go
package main

import (
	"encoding/csv"
	"fmt"
	"log"
	"os"
	"strings"
)

func main() {
	// CSVデータの書き込み
	csvData := `Name, Age, City
Alice, 25, New York
Bob, 30, Los Angeles`
	csvFile, err := os.Create("example.csv")
	if err != nil {
		log.Fatalf("CSVファイル作成エラー: %v", err)
	}
	defer csvFile.Close()
	csvFile.Write([]byte(csvData))

	// CSVデータの読み込み
	csvFile, err = os.Open("example.csv")
	if err != nil {
		log.Fatalf("CSVファイル開くエラー: %v", err)
	}
	defer csvFile.Close()

	reader := csv.NewReader(csvFile)
	records, err := reader.ReadAll()
	if err != nil {
		log.Fatalf("CSV読み込みエラー: %v", err)
	}

	for _, record := range records {
		fmt.Println(strings.Join(record, ", "))
	}
}
```

実行結果:
```
Name, Age, City
Alice, 25, New York
Bob, 30, Los Angeles
```

## Deep Dive
CSVは1972年にIBMで使われ始め、データ交換の簡素さのためすぐに普及しました。他のフォーマット、例えばJSONやXMLも人気がありますが、CSVはそのシンプルさで特に大量のデータを扱う際に有利です。Goの`encoding/csv`パッケージは、RFC 4180に準拠していますが、柔軟に設定変更可能です。

## See Also
- Goの公式ドキュメントのCSVパッケージ: https://pkg.go.dev/encoding/csv
- CSVデータ形式の詳細(RFC 4180): https://tools.ietf.org/html/rfc4180
- Goによるデータ処理に関するチュートリアル: https://golang.org/doc/articles/csv_and_json
