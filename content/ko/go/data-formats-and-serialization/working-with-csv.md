---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:18.011117-07:00
description: "\uCF64\uB9C8\uB85C \uAD6C\uBD84\uB41C \uAC12(CSV) \uD615\uC2DD\uC740\
  \ \uADF8 \uB2E8\uC21C\uC131\uACFC \uB300\uBD80\uBD84\uC758 \uD504\uB85C\uADF8\uB798\
  \uBC0D \uC5B8\uC5B4\uC640\uC758 \uC26C\uC6B4 \uD1B5\uD569 \uB355\uBD84\uC5D0 \uB370\
  \uC774\uD130 \uAD50\uD658\uC744 \uC704\uD574 \uB9CC\uC5F0\uD558\uAC8C \uC0AC\uC6A9\
  \uB429\uB2C8\uB2E4. \uC774\uC5D0 \uD574\uB2F9\uD558\uB294 \uC5B8\uC5B4\uB85C\uB294\
  \ Go\uB3C4 \uD3EC\uD568\uB429\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uB370\uC774\uD130 \uB9C8\uC774\uADF8\uB808\uC774\uC158, \uBCF4\uACE0\uC11C \uC0DD\
  \uC131 \uB610\uB294 \uB370\uC774\uD130 \uBD84\uC11D\uC744 \uC704\uD574 \uC885\uC885\
  \ CSV \uD30C\uC77C\uC744\u2026"
lastmod: '2024-03-13T22:44:54.495567-06:00'
model: gpt-4-0125-preview
summary: "\uCF64\uB9C8\uB85C \uAD6C\uBD84\uB41C \uAC12(CSV) \uD615\uC2DD\uC740 \uADF8\
  \ \uB2E8\uC21C\uC131\uACFC \uB300\uBD80\uBD84\uC758 \uD504\uB85C\uADF8\uB798\uBC0D\
  \ \uC5B8\uC5B4\uC640\uC758 \uC26C\uC6B4 \uD1B5\uD569 \uB355\uBD84\uC5D0 \uB370\uC774\
  \uD130 \uAD50\uD658\uC744 \uC704\uD574 \uB9CC\uC5F0\uD558\uAC8C \uC0AC\uC6A9\uB429\
  \uB2C8\uB2E4."
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

## 방법:
Go에서 CSV 파일을 다루는 것은 표준 라이브러리 `encoding/csv` 덕분에 간단합니다. 아래는 CSV 파일을 읽고 쓰는 방법에 대한 기초입니다.

### CSV 파일 읽기
CSV 파일에서 읽기 위해, 먼저 `os.Open`을 사용하여 파일을 열고, `csv.NewReader`로 새 CSV 리더를 생성합니다.

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
        panic(err)
    }
    defer file.Close()

    reader := csv.NewReader(file)
    records, err := reader.ReadAll()
    if err != nil {
        panic(err)
    }

    for _, record := range records {
        fmt.Println(record)
    }
}
```

이 코드 예제는 `data.csv`에서 모든 레코드를 읽고 출력합니다. 각 레코드는 필드의 슬라이스입니다.

### CSV 파일 쓰기
쓰기를 위해서는, 여러 또는 단일 CSV 레코드를 각각 쓸 때 `csv.NewWriter`와 `writer.WriteAll` 혹은 `writer.Write`를 사용합니다.

```go
package main

import (
    "encoding/csv"
    "os"
)

func main() {
    file, err := os.Create("output.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    writer := csv.NewWriter(file)
    defer writer.Flush()

    records := [][]string{
        {"Name", "Age", "City"},
        {"John Doe", "30", "New York"},
        {"Jane Doe", "27", "Los Angeles"},
    }

    if err := writer.WriteAll(records); err != nil {
        panic(err)
    }
}
```

이것은 제공된 레코드로 `output.csv`라는 파일을 생성할 것입니다. 모든 버퍼링된 데이터가 파일로 쓰여지도록 작성자를 플러시하는 것을 항상 기억하세요.

## 심층 탐구
Go의 `encoding/csv` 패키지는 CSV 파일을 읽고 쓸 수 있는 강력한 지원을 제공하지만, 단순성을 염두에 두고 설계되었기 때문에 구분자의 자동 감지, 따옴표 또는 필드 내에 임베드된 줄바꿈을 처리하는 것과 같은 더 복잡한 시나리오는 수동 처리 없이는 다루지 않습니다.

역사적으로, 프로그래밍 언어에서 CSV 처리는 이러한 복잡성 때문에 종종 번거로웠지만, Go의 표준 라이브러리는 이러한 문제를 많이 추상화하여 개발자가 CSV 데이터를 상대적으로 쉽게 다루게 합니다. 그러나, 더 복잡한 CSV 조작을 위해서는 `gocsv`와 같은 제3자 라이브러리를 사용하거나 수동으로 구문 분석을 처리해야 할 수도 있습니다.

Go의 `csv` 패키지의 주목할 만한 측면은 맞춤형 쉼표(구분자)를 지정할 수 있어, 탭으로 구분된 값(TSV)과 같은 CSV 파일의 변형과 원활하게 작동할 수 있다는 것입니다. 그러나 매우 불규칙하거나 비표준 CSV 파일을 처리할 때, Go 프로그래머는 기존 csv 리더나 작성자 구현을 확장해야 할 필요성을 느낄 수 있습니다.

일반적인 용도로는 Go의 CSV 처리 기능이 견고하지만, 데이터 과학이나 복잡한 데이터 변환 작업과 같이 집중적인 데이터 조작이 필요한 응용 프로그램의 경우, 프로그래머는 전용 데이터 처리 패키지를 살펴보거나, 이러한 작업에 더 적합한 다른 언어, 예를 들어 `pandas` 라이브러리가 있는 Python을 고려할 수 있습니다. 그럼에도 불구하고, 간단한 CSV 읽기-쓰기 작업의 경우, Go의 표준 라이브러리는 그 효율과 단순함으로 눈에 띕니다.
