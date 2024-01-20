---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:36:31.660748-07:00
html_title:           "Arduino: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

문자열에서 날짜 파싱은 문자로 된 날짜 정보를 프로그램이 이해할 수 있게 변환하는 것입니다. 데이터 처리, 로깅, 혹은 사용자 인터페이스 상의 날짜 입력 때문에 프로그래머는 이 작업을 합니다.

## How to: (어떻게 하나요?)

Gleam에서 날짜를 파싱하는 데 정규식(Regex)를 사용할 수 있습니다. 이 예는 간단한 YYYY-MM-DD 포맷을 파싱합니다.

```gleam
import gleam/regex.{find}

pub fn parse_date(date_string: String) -> Result(String, Nil) {
  let date_pattern = "^\\d{4}-\\d{2}-\\d{2}$"
  case find(date_pattern, date_string) {
    Ok(_) -> Ok(date_string)
    Error(e) -> Error(e)
  }
}

fn main() {
  let date = "2023-03-15"
  parse_date(date)
}

// 예상 출력: Ok("2023-03-15")
```

## Deep Dive (심층 분석)

날짜 문자열 파싱은 컴퓨터 프로그램이 시작된 이래로 존재해 왔습니다. 표준화된 날짜와 시간 포맷인 ISO 8601이 널리 쓰여집니다. Gleam언어에서는 `regex` 라이브러리 이외에도 `calendar` 라이브러리를 사용하여 날짜를 처리할 수 있습니다. `regex`는 단순한 패턴 인식이고, `calendar`는 실제 날짜 연산을 허용합니다. 파싱에는 실행 퍼포먼스가 중요한데, 정규식이 항상 최고의 성능을 내는 것은 아니므로 상황에 따라 적절한 도구를 선택해야 합니다.

## See Also (참고 자료)

- Gleam 공식 문서: [https://gleam.run](https://gleam.run)
- 정규 표현식에 대한 튜토리얼: [https://www.regular-expressions.info](https://www.regular-expressions.info)
- ISO 8601 표준에 대한 정보: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)