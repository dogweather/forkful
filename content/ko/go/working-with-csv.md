---
title:                "csv 파일 작업하기"
html_title:           "Go: csv 파일 작업하기"
simple_title:         "csv 파일 작업하기"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜

CSV 파일은 데이터를 저장하거나 전달하는 데 자주 사용되는 형식입니다. Go 언어는 CSV 파일을 다루기에 아주 효율적이며, 이를 활용하여 데이터 분석, 웹 개발 등 다양한 작업을 할 수 있습니다.

## 하는 법

CSV 파일을 읽고 쓰기 위해서는 "encoding/csv" 패키지를 임포트해야 합니다. 파이썬의 코드와 비슷하게 매우 간단합니다. 예를 들어, 다음과 같은 CSV 파일이 있다고 가정해 봅시다.

```Go
1,Apple,Red
2,Orange,Orange
3,Banana,Yellow
```

우리는 이 파일을 열어서 각 행을 슬라이스로 저장하고 싶을 겁니다. 이를 위해 다음과 같이 코드를 작성할 수 있습니다.

```Go
package main

import (
	"encoding/csv"
	"log"
	"os"
)

func main() {
	// CSV 파일 열기
	file, err := os.Open("fruits.csv")
	if err != nil {
		log.Fatal(err)
	}

	// CSV 리더 생성
	reader := csv.NewReader(file)

	rows, err := reader.ReadAll()
	if err != nil {
		log.Fatal(err)
	}

	// 각 행을 슬라이스로 저장
	var fruits [][]string
	for _, row := range rows {
		fruits = append(fruits, row)
	}

	// 슬라이스 내용 출력
	for _, fruit := range fruits {
		log.Println(fruit)
	}
}

```

위 코드를 실행하면 다음과 같은 결과를 얻을 수 있습니다.

```Go
[1 Apple Red]
[2 Orange Orange]
[3 Banana Yellow]
```

CSV 파일을 쓰는 과정도 매우 간단합니다. 다음과 같이 코드를 작성할 수 있습니다.

```Go
package main

import (
	"encoding/csv"
	"log"
	"os"
)

func main() {
	// CSV 파일 생성
	file, err := os.Create("new_fruits.csv")
	if err != nil {
		log.Fatal(err)
	}

	// CSV 라이터 생성
	writer := csv.NewWriter(file)
	defer writer.Flush()

	// 데이터 슬라이스 생성
	data := [][]string{
		{"1", "Apple", "Red"},
		{"2", "Orange", "Orange"},
		{"3", "Banana", "Yellow"},
	}

	// 슬라이스 데이터를 CSV 파일에 쓰기
	err = writer.WriteAll(data)
	if err != nil {
		log.Fatal(err)
	}
}
```

위 코드를 실행하면 "new_fruits.csv" 파일이 생성되고, 그 안에 다음과 같은 내용이 들어있을 것입니다.

```
1,Apple,Red
2,Orange,Orange
3,Banana,Yellow
```

마지막으로, 데이터 분석을 위해 CSV 파일을 읽는 데에는 "encoding/csv" 패키지 외에도 "encoding","strconv" 등의 다양한 내장 패키지들을 활용할 수 있습니다.

## 딥 다이브

CSV 파일은 일반적으로 쉽게 다루어지기 때문에 Go 언어에서도 매우 유용하게 활용할 수 있습니다. 다만 주의할 점이 있다면, CSV 파일의 데이터 타입은 모두 문자열로 인식되기 때문에 숫자 데이터를 다루는 경우 "strconv" 패키지를 활용하여 형변환을 해주어야 하는 점입니다. 또한, CSV 파일을 쓸 때 데이터가 올바른 형식을 가지고 있는지 확인하는 코드를 추가하는 것도 좋은 방법입니다.

## 참고 자료

- "encoding/csv" 패키지: https://golang.org/pkg/encoding/csv/
- 다른 내장 패키지들의 사용 예시: https://golang.org/pkg/

---

## 참고 자료

- "encoding/csv" 패키지: