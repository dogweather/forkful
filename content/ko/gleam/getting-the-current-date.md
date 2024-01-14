---
title:                "Gleam: 현재 날짜 받기"
simple_title:         "현재 날짜 받기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

날짜를 얻는 것에 대한 의미는 대부분의 사람들에게 명확합니다. 우리는 우리의 일정, 회의 및 이벤트를 추적하며 기한을 준수할 필요가 있기 때문에 현재 날짜를 알고 싶어합니다. 따라서 모든 프로그래밍 언어에는 현재 날짜를 얻는 기능이 있으며 Gleam 역시 예외는 아닙니다.

## 어떻게

Gleam에서 현재 날짜를 얻기 위해서는 `gleam/time` 모듈을 임포트해야 합니다. 그런 다음 `Time.now()` 함수를 호출하면 현재 시간이 `Time` 구조체로 반환됩니다.

예를 들어, 다음과 같은 코드를 실행하면 현재 날짜와 시간이 출력됩니다.

```Gleam
import gleam/time

fn main() {
  time = Time.now()
  IO.print(time)
}
```

출력:

```Gleam
#<Time { 
  day: 3, # day of the month
  day_of_week: Fri,
  day_of_year: 316,
  hour: 16,
  microsecond: 259625,
  minute: 10,
  month: Nov,
  nanosecond: 259625000,
  second: 53,
  timezone: +00,
  year: 2021
}>
```

Gleam에서는 `Time` 구조체를 사용하여 원하는 포맷으로 날짜와 시간을 출력할 수도 있습니다. 예를 들어, 다음과 같은 코드로 현재 날짜와 시간을 `yyyy-MM-dd hh:mm:ss` 형식으로 출력할 수 있습니다.

```Gleam
import gleam/time

fn main() {
  time = Time.now()
  datetime = Time.format(time, "%Y-%m-%d %H:%M:%S")
  IO.print(datetime)
}
```

출력:

```Gleam
2021-11-03 16:17:54
```

## 딥 다이브

Gleam에서 날짜와 시간을 다루는 더욱 심화된 내용을 알아봅시다. `Time` 구조체는 12개의 필드를 가지고 있습니다. 각 필드의 의미는 다음과 같습니다.

- `day`: 일
- `day_of_week`: 일주일 중 몇 번째 요일인지 나타냄 (Mon, Tue, Wed, Thu, Fri, Sat, Sun)
- `day_of_year`: 일년 중 몇 번째 날인지 나타냄
- `hour`: 시간 (24시간 형식)
- `microsecond`: 마이크로초
- `minute`: 분
- `month`: 월 (1월부터 12월까지)
- `nanosecond`: 나노초
- `second`: 초
- `timezone`: 시간대 (현재는 +00 고정)
- `year`: 연도

현재 날짜와 시간을 `Time` 구조체의 각 필드별로 어떻게 얻을 수 있는지 살펴봅시다.

- `day`: `time.day`
- `day_of_week`: `time.day_of_week`
- `day_of_year`: `time.day_of_year`
- `hour`: `time.hour`
- `microsecond`: `time.microsecond`
- `minute`: `time.minute`
- `month`: `time.month`
- `nanosecond`: `time.nanosecond`
- `second`: `time.second`
- `timezone`: `time.timezone`
- `year`: `time.year`

## 따라 해보기

마지막으로, Gleam에서 현재 날짜와 시간을 얻는 방법을 직접 해보세요. Gleam REPL에서 간단한 코드를 실행해보거나, 새로운 프로젝트를 시작하여 날짜와 시간을 출력하는 코드를 작성