---
title:                "Gleam: CSV 작업하기"
simple_title:         "CSV 작업하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜

오늘 우리는 CSV 파일에 대해서 배우려고 합니다. CSV 파일은 데이터를 쉽게 저장할 수 있는 파일 형식으로 많은 프로그래밍 언어에서 지원되는 일반적인 형식입니다. CSV 파일을 다루는 것은 데이터를 다루는 데 있어서 중요한 스킬이 될 수 있으며, 많은 프로젝트에서 필수적인 요소가 될 수 있습니다.

## 하우 투

Gleam은 `gleam_csv` 패키지를 통해 CSV 파일을 다룰 수 있도록 지원합니다. 이를 사용하기 위해서는 먼저 `gleam_csv` 패키지를 프로젝트에 추가해야 합니다. 그리고 아래 예시 코드를 참고하여 CSV 파일을 읽고 쓰는 방법을 배워보세요.

 ```Gleam
import gleam/csv

// CSV 파일 읽기
let result = csv.decode_file("data.csv")
match result {
  Ok(rows) -> {
    // 각 행(row)은 열(column)의 배열로 구성됩니다.
    for row in rows {
      // 각 열의 값에 접근하기 
      let name = row[0]
      let age = row[1]
      let occupation = row[2]
      // 원하는 작업 수행하기
    }
  }
  Err(error) -> {
    // 오류 처리하기
  }
}

// CSV 파일 쓰기
let records = [ ["John", "25", "Student"], ["Jane", "30", "Teacher"] ]

let result = csv.encode_file("data.csv", records)
match result {
  Ok(_) -> { /* 파일이 성공적으로 저장됨 */ }
  Err(error) -> { /* 오류 처리하기 */ }
}
 ```

## 딥 다이브

CSV 파일을 다룰 때 주의해야할 몇 가지 사항이 있습니다. 첫 번째로, CSV 파일에는 파일 형식에 대한 정보가 포함되어 있지 않습니다. 따라서 파일이 어떤 형식인지 명시해주어야 합니다. 예를 들어, 첫 번째 행에 열(column)의 제목을 포함시켜야 합니다. 또한 첫 번째 행은 데이터가 아니기 때문에 주의해서 처리해야 합니다.

두 번째로, CSV 파일의 값은 모두 문자열로 저장되기 때문에 숫자를 다룰 때 유의해야 합니다. 필요에 따라서는 숫자로 변환해주어야 할 수도 있습니다.

마지막으로, CSV 파일을 다룰 때는 예외 처리가 중요합니다. 파일을 읽거나 쓸 때 오류가 발생할 수 있기 때문에 적절한 예외 처리를 해주어야 합니다.

## 참고

- Gleam 공식 문서: https://gleam.run/documentation/
- Gleam CSV 패키지: https://github.com/gleam-lang/csv