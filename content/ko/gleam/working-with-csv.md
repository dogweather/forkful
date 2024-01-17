---
title:                "CSV 작업"
html_title:           "Gleam: CSV 작업"
simple_title:         "CSV 작업"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

[신규 버전의 Gleam 프로그래밍] 

# Gleam에서 CSV 작업하기

## 무엇과 왜?
CSV란 무엇일까요? CSV는 Comma Separated Values의 약자로, 각각의 값이 쉼표로 구분되어 있는 데이터 형식을 말합니다. 프로그래머들은 이러한 CSV 형식의 데이터를 다룰 때 자주 사용하는데, 이는 데이터의 읽기와 쓰기, 그리고 분석 등 다양한 작업을 쉽게 할 수 있기 때문입니다.

## 작업 방법:
아래의 코드 블록을 사용하여 Gleam에서 CSV 파일을 다루는 방법을 알아보세요.

Gleam 코드 예제:

```Gleam
// CSV 파일 읽기
let csv = import("my_data.csv")
let data = csv |> io.read_csv
// data는 CSV 파일의 각각의 행을 구성하는 리스트를 포함합니다. 

// CSV 파일 쓰기
let my_data = ["apple", "banana", "orange"]
let data = my_data |> io.to_csv()
// data는 쉼표로 구분된 apple, banana, orange로 이루어진 문자열을 포함합니다.
```

## 깊이 들어가기:
CSV 파일 형식은 1980년대에 최초로 개발되었습니다. 당시에는 컴퓨터 간에 데이터를 공유하기 위해 만들어졌는데, 지금도 여러 가지 형태의 데이터를 교환하는 데 많이 사용됩니다. Gleam 외에도 다양한 프로그래밍 언어에서 CSV를 다루는 다양한 라이브러리가 제공되고 있습니다. Gleam에서는 io 라이브러리를 사용하여 CSV를 손쉽게 다룰 수 있습니다.

## 관련 자료:
- [io 라이브러리 문서](https://gleam.run/documentation/stdlib/io)
- [CSV 형식 소개](https://en.wikipedia.org/wiki/Comma-separated_values)