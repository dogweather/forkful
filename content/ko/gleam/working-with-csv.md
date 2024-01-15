---
title:                "CSV 작업하기"
html_title:           "Gleam: CSV 작업하기"
simple_title:         "CSV 작업하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜?

CSV 파일은 데이터를 저장하고 공유하기에 효율적이고 편리한 형식입니다. 지속적으로 커뮤니케이션하거나 다른 프로그램과 데이터를 공유하려는 경우, CSV 파일 형식은 매우 유용합니다.

## 어떻게 하나요?

```Gleam
import gleam/csv

csv_string = "name,age\nJohn,25\nJane,30"

// CSV 문자열을 인코딩합니다.
encoded_csv = csv.encode(csv_string)

// CSV 파일을 읽어서 데이터를 매핑합니다.
decoded_csv = csv.read_file("data.csv", headers: true)

// 특정 필드 값을 추출합니다.
name = decoded_csv[0]["name"] // "John"
```

## 깊이 파고들기

데이터 분석, 추출 및 변환에 유용한 기능들이 많이 포함된 Gleam의 CSV 라이브러리는 사용하기 매우 간단합니다. 필요한 경우, 커스터마이즈된 분리 문자, 헤더 옵션 등 다양한 설정을 적용할 수 있습니다. 더 자세한 내용은 [공식 문서](https://gleam.run/packages/csv.html)를 참조해주세요.

## 더 알아보기

- [Gleam 문서](https://gleam.run/)에서 언어의 기본적인 문법과 라이브러리에 대해 더 알아보세요.
- [CSV 파일 형식](https://en.wikipedia.org/wiki/Comma-separated_values)에 대해 더 깊이 알아보세요.