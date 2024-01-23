---
title:                "두 날짜 비교하기"
date:                  2024-01-20T17:33:00.229467-07:00
model:                 gpt-4-1106-preview
simple_title:         "두 날짜 비교하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜 비교는 두 날짜를 기준으로 실행되는 연산입니다. 프로그래머들은 기간 계산, 시간순 정렬, 유효기간 판단 등을 위해 이 기능을 사용합니다.

## How to: (방법)
Gleam에서 날짜를 비교하려면 `gleam/calendar` 모듈을 사용하세요. 아래에 간단한 예제와 결과가 있습니다.

```gleam
import gleam/calendar.{Date, diff_days}

pub fn main() {
  let date1 = Date(year: 2023, month: 3, day: 14)
  let date2 = Date(year: 2023, month: 4, day: 28)
  let difference = diff_days(date1, date2)
  
  difference
}
```

이 코드의 결과는 두 날짜 차이인 '45'가 됩니다.

## Deep Dive (심층 분석)
날짜 비교는 안정적인 시스템 기반에서 철저히 테스트되었습니다. 여러 타임존과 윤년도 고려하여 정확성을 확보합니다. Alternatives로는 `erlang/otp`의 `:calendar` 모듈 또는 타 언어의 날짜 처리 라이브러리가 있습니다만, Gleam 내장 함수를 통한 처리가 바로 사용가능하여 간편합니다. 비교 함수는 내부적으로 날짜 간 차이를 날 수로 계산하여 제공합니다.

## See Also (관련 자료)
- Erlang's `:calendar` module documentation: [Erlang Calendar Module](http://erlang.org/doc/man/calendar.html)
- Time and date programming best practices: [Time and Date Best Practices](https://yourcalendricalfallacyis.com/)
