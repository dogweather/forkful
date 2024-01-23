---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:14:43.874847-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
현재 날짜 가져오기란 시스템에서 지금 날짜와 시간을 찾는 것입니다. 프로그래머는 로그, 사용자 이벤트 추적 또는 콘텐츠 타임스탬프와 같은 기능에 이를 사용합니다.

## How to: (어떻게 하기:)
Gleam에서 현재 날짜와 시간을 얻기 위해, 우리는 표준 라이브러리를 사용합니다. 예제와 출력을 봅시다.

```gleam
import gleam/calendar.{Year, Month, Day, Hour, Minute, Second, Datetime}
import gleam/erlang

pub fn get_current_datetime() -> Result(Datetime, String) {
  erlang.now()
  |> datetime.from_timestamp()
}

pub fn main() {
  let Ok(datetime) = get_current_datetime()
  datetime
}
```

실행 예시:

```
# Output
Ok(
  Datetime(
    Year(2023),
    Month(4),
    Day(11),
    Hour(14),
    Minute(9),
    Second(26),
  )
)
```

## Deep Dive (깊은 탐구)
과거엔 현재 날짜를 얻기 위해 여러 언어와 라이브러리에서 다르게 접근해왔습니다. 다른 언어에는 다른 기능이 있지만, Gleam은 `erlang.now()`와 `datetime.from_timestamp()`를 제공하여 간단하게 현재의 날짜와 시간 정보를 얻을 수 있습니다.

시간 데이터를 다룰 때 주의할 점은, Gleam은 UTC(협정 세계시) 기준의 값으로 결과를 생성한다는 것입니다. 로컬 타임존에 맞추려면 추가 변환 작업을 해야 합니다.

`erlang.now()` 함수는 내부적으로 Erlang의 시스템 클록을 사용합니다. 이것은 강력하지만, 시스템의 시간 설정이 잘못되었을 때 문제가 발생할 수 있습니다.

## See Also (관련 자료)
- Gleam 공식 문서: [https://gleam.run](https://gleam.run)
- Erlang 시간 관련 기능 문서: [http://erlang.org/doc/man/erlang.html](http://erlang.org/doc/man/erlang.html)
- UTC와 타임존에 대한 추가 정보: [https://www.worldtimeserver.com](https://www.worldtimeserver.com)
