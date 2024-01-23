---
title:                "미래나 과거의 날짜 계산하기"
date:                  2024-01-20T17:30:50.940358-07:00
model:                 gpt-4-1106-preview
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜 계산은 특정 날짜에 일정 기간을 더하거나 빼는 것입니다. 프로그래머들은 기한 정하기, 이벤트 스케줄링 또는 날짜차이 계산 시 이를 사용합니다.

## How to: (어떻게:)
```Gleam
import gleam/calendar.{Date, add_days}

pub fn main() {
  // 오늘 날짜: 2023년 4월 1일
  let today = Date(year: 2023, month: 4, day: 1)
  
  // 미래 날짜 계산: 오늘로부터 10일 후
  let future_date = today |> add_days(10)
  // 출력: 2023년 4월 11일
  
  // 과거 날짜 계산: 오늘로부터 10일 전
  let past_date = today |> add_days(-10)
  // 출력: 2023년 3월 22일
  
  future_date
  past_date
}
```

## Deep Dive (심층 탐구)
옛날부터 사람들은 시간을 추적하며 농사, 종교 의식, 해상 네비게이션과 같은 활동을 위해 날짜를 계산해왔습니다. 컴퓨터 프로그래밍에서 날짜 계산은 더 정교해졌습니다. 다른 프로그래밍 언어마다 내장된 날짜 처리 라이브러리가 있어요. 예를 들어, 자바에는 `java.util.Calendar`, 파이썬에는 `datetime` 모듈이 있죠. Gleam에서는 `gleam/calendar` 라이브러리를 사용합니다.

Gleam에서 날짜를 처리할 때, 표준 시간대 변화나 윤초 같은 복잡한 시간 이슈를 고려하지 않는다면 `Date` 구조체와 `add_days` 함수를 사용하는 것으로 충분합니다. 더 복잡한 시간 계산이 필요하다면 외부 라이브러리를 찾아볼 수 있어요. 

Gleam은 타입 안정성과 명확한 코드를 중시하기에 날짜와 시간 계산 시 오류를 줄여주고, 의도치 않은 버그를 방지하는 이점을 제공합니다.

## See Also (관련 자료)
- 동기적 시간 계산에 관한 더 깊은 이해를 위한 NTP(Network Time Protocol) 설명: [https://www.ntp.org](https://www.ntp.org)
