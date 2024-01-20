---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

문자열에서 날짜를 분석하면, 우리는 텍스트 형식을 특정 날짜 형식으로 변환할 수 있습니다. 이 기능은 프로그래머들에게 중요하다. 왜냐하면 데이터는 종종 다른 형식으로 제공되기 때문이며, 최종 사용자가 날짜를 인식하게 하기 위해서는 특정 형식으로 변환할 필요가 있습니다.

## 어떻게:

```Gleam
import gleam/otp/calendar.{parse}
import gleam/result.{unwrap}

let date_string = "2022-09-12"
let date_format = "{YYYY}-{MM}-{DD}"
let parsed_date = unwrap(parse(date_format, date_string))
```
이것을 출력하면 당신은 이렇게 볼 수 있습니다: 
```Gleam
{ok, #calendar.date(year: 2022, month: 9, day: 12)}
```

## 조금 더 알아보기:

기본적으로, 문자열에서 날짜를 분석하는 것은 오래된 문제이며, 이러한 기능은 대부분의 프로그래밍 언어에 내장되어 있습니다. Gleam에서는 기본 내장 라이브러리인 `gleam/otp/calendar`를 사용하여 이 기능을 제공합니다.

다른 대안으로는 `parse_any` 함수를 사용하는 것이 있습니다. `parse_any`는 다양한 형식의 날짜 문자열로부터 날짜를 추출할 수 있습니다. 또한, 당신의 코드가 특정 입력 형식에 덜 종속적이게 만듭니다.

문자열에서 날짜를 분석할 때 분석 오류가 자주 발생합니다. 이 경우, `parse` 함수는 Err 값을 반환합니다. 따라서 항상 반환 값을 확인하고 적절히 처리하는 것이 중요합니다.

## 참고 자료:

Gleam 날짜 관련 패키지: https://hexdocs.pm/gleam_otp/calendar.html

알고리즘에 대한 더 깊은 이해: https://en.wikipedia.org/wiki/Date_format_by_country

다양한 프로그래밍 언어에서 날짜 구문 분석에 대한 여러 팁: https://stackoverflow.com/questions/2158347/how-do-i-parse-a-string-into-a-datetime-in-different-languages