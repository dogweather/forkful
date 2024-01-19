---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "Gleam: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
미래나 과거의 날짜를 계산하는 것은 특정 날짜로부터 지정된 기간동안의 날짜를 찾는 과정입니다. 개발자들이 이를 수행하는 이유는 다양한 시간기반 기능을 제공하기 위함입니다, 시간차에 따른 Reminder 기능이나 일정 기록과 같은 것들이 대표적인 예시입니다.

## 어떻게
```Gleam
pub import gleam/calendar.{da,da_diff}
import gleam/io

pub fn main() {
  let today = da.today()
  let month_from_now = da.add_months(today, 1)
  let diff = da_diff.months(today, month_from_now)

  io.println(month_from_now)
  io.println(diff)
}
```
위 코드를 실행하면 아래와 같이 오늘로부터 한 달 후의 날짜와 그 두 날짜 사이의 차이를 얻을 수 있습니다:
```
{year: 2023, month: 8, day: 21}
1
```

## 깊게 알기
날짜 계산은 프로그래밍의 초기 시기부터 필요했던 주요 기능 중 하나입니다. 이 기능은 특히나 일정 관리, 예약 시스템, 프로젝트 관리 등에서 중요하게 사용되었습니다. 물론, Gleam 외에도 Python의 `datetime`, Javascript의 `date-fns`와 같은 다양한 언어와 라이브러리에서 날짜 계산 기능을 제공하고 있습니다. `gleam/calendar` 라이브러리에서는 기본적인 단위를 day, month, year로 설정하여 기간계산을 제공하고 있습니다.

## 관련 자료
- Gleam 공식 문서: https://gleam.run/docs/
- `gleam/calendar` 라이브러리 문서: https://hexdocs.pm/gleam_stdlib/gleam/calendar.html
- Python의 datetime: https://docs.python.org/3/library/datetime.html
- Javascript의 date-fns: https://date-fns.org/