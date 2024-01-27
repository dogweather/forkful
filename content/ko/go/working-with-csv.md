---
title:                "CSV 파일 다루기"
date:                  2024-01-19
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

CSV는 Comma-Separated Values를 의미하며 데이터를 표 형식으로 저장합니다. 프로그래머들은 데이터 교환과 처리에 CSV를 사용하며, 간편하고 호환성이 높아 많이 사용됩니다.

## How to: (어떻게 하나요?)

```Go
package main

import (
	"encoding/csv"
	"fmt"
	"os"
	"strings"
)

func main() {
	// 쓰기 예제
	csvContent := [][]string{
		{"name", "age", "city"},
		{"Alice", "25", "New York"},
		{"Bob", "30", "San Francisco"},
	}

	csvFile, err := os.Create("example.csv")
	if err != nil {
		panic(err)
	}
	defer csvFile.Close()

	writer := csv.NewWriter(csvFile)
	for _, row := range csvContent {
		if err := writer.Write(row); err != nil {
			panic(err) // row 쓰기 실패
		}
	}
	writer.Flush()

	// 읽기 예제
	csvFile, err = os.Open("example.csv")
	if err != nil {
		panic(err)
	}
	defer csvFile.Close()

	reader := csv.NewReader(csvFile)
	records, err := reader.ReadAll()
	if err != nil {
		panic(err) // CSV 읽기 실패
	}

	for _, record := range records {
		fmt.Println(strings.Join(record, ", "))
	}
}
```

출력 예제:

```
name, age, city
Alice, 25, New York
Bob, 30, San Francisco
```

## Deep Dive (심층 분석)

CSV는 1972년 IBM에서 사용하기 시작했고, 가장 단순한 텍스트 기반 데이터 형식 중 하나입니다. JSON이나 XML 같은 대안들도 있지만, CSV는 가벼움과 호환성 때문에 여전히 인기가 많습니다. Go에서는 `encoding/csv` 패키지를 이용해 CSV 읽기 및 쓰기를 간단히 수행할 수 있으며, 네트워크 데이터 전송, 파일 조작 등 다양한 상황에 사용됩니다.

## See Also (더 보기)

- Go언어 공식 문서 내 CSV 패키지 설명 [encoding/csv](https://golang.org/pkg/encoding/csv/)
