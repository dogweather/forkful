---
title:                "csv 작업하기"
html_title:           "Go: csv 작업하기"
simple_title:         "csv 작업하기"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
CSV 파일은 쉼표로 구분된 값을 저장하는 파일 형식입니다. 프로그래머들은 이 파일을 사용하며 데이터를 효율적으로 관리할 수 있습니다.

## 방법:
CSV 파일을 처리하는 방법은 간단합니다. 우선, "encoding/csv" 패키지를 import 해야 합니다. 그 후, 다음과 같은 코드를 사용하여 파일을 열고 처리할 수 있습니다.
```
file, err := os.Open("data.csv")
if err != nil {
    log.Fatal(err)
}
defer file.Close()

reader := csv.NewReader(file)
records, err := reader.ReadAll()
if err != nil {
    log.Fatal(err)
}

for _, record := range records {
    fmt.Println(record)
}
```
위 코드는 CSV 파일을 읽고 각 라인을 출력합니다.

## 깊이 빠져들기:
CSV 파일은 1970년대 초기 표를 저장하기 위해 만들어졌습니다. 이 후로도 여전히 널리 사용되고 있지만 XML, JSON 등의 대안들이 있습니다. Go의 "encoding/csv" 패키지는 RFC 4180에 따라 제작되었습니다. CSV 파일을 처리할 때 유의해야 할 점으로는 모든 파일이 제목, 라인 수 등의 공통 구문을 가지고 있지는 않다는 것입니다.

## 관련 자료:
- [CSV 파일 형식 설명서](https://tools.ietf.org/html/rfc4180)
- [Go의 encoding/csv 패키지 문서](https://golang.org/pkg/encoding/csv/)