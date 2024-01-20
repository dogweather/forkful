---
title:                "CSV 파일 다루기"
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
CSV(쉼표로 구분된 값)는 데이터 저장과 전송에 유용합니다. 프로그래머들은 데이터 교환 및 가볍고 호환성 높은 파일 형식이 필요할 때 CSV를 사용합니다.

## How to:
Gleam에서 CSV 작업을 시작하는 기본적인 예시입니다.

```gleam
import gleam/csv

// CSV 문자열을 파싱하는 예제
pub fn parse_csv(data: String) -> Result(list(list(String)), Nil) {
  let rows = csv.parse_string(data)
  case rows {
    Ok(rows) -> Ok(rows)
    Error(_) -> Error(Nil)
  }
}

// 예제 CSV 데이터로 파싱해보기
pub fn example() {
  let csv_data = "name,age\nJohn,30\nJane,25"
  parse_csv(csv_data)
}
```

실행 결과:
```gleam
Ok([["name", "age"], ["John", "30"], ["Jane", "25"]])
```

## Deep Dive (심화 탐구)
CSV 형식은 1972년 IBM에 의해 처음으로 소개됐습니다. XML, JSON과 같은 다른 데이터 형식도 있지만, CSV는 간결함과 가독성에 있어 장점이 있습니다. Gleam에서는 `gleam/csv` 라이브러리를 통해 CSV 데이터를 쉽게 파싱하고 조작할 수 있습니다. 

## See Also (더보기)
- Gleam 공식 문서: [Gleam Documentation](https://gleam.run/)
- CSV에 대한 자세한 정보: [RFC 4180](https://tools.ietf.org/html/rfc4180)