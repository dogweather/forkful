---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

두 날짜를 비교한다는 것은 두 시간점의 순서나 간격을 파악하는 것입니다. 프로그래머는 이것을 사용하여 이벤트가 발생한 순서를 추적하거나 시간 기반 논리를 적용하는 등의 작업을 수행합니다.

## 어떻게?

```Gleam
import gleam/date.{Date, equal, after, before}
let today = date.new(2022, 10, 3)
let tomorrow = date.new(2022, 10, 4)

let _ = assert equal(today, tomorrow) == False
let _ = assert after(today, tomorrow) == False
let _ = assert before(today, tomorrow) == True
```
위의 예시에서, 우리는 오늘과 내일 두 날짜를 생성하고 비교합니다. `equal`, `after`, `before` 함수들은 각각 두 날짜가 같은지, 첫 번째 날짜가 두 번째보다 나중인지, 첫 번째 날짜가 두 번째보다 이른지를 검사합니다.

## 깊이 들여다보기

날짜 비교는 프로그래밍 언어의 역사와 깊이 연관되어 있습니다. 초기 프로그래밍 언어들은 날짜 비교 기능을 내장하지 않았고 프로그래머들이 직접 시간 연산을 수행해야 했습니다.

Gleam에서 날짜를 비교하는 것은 내부적으로 Unix 타임스탬프를 사용합니다. 이는 1970년 1월 1일부터 지난 시간을 초로 계산한 값입니다. 위와 같은 방식은 표준화가 잘 되어 있으며, 많은 프로그래밍 언어에서 지원하고 있습니다.

비교에 관한 간결함과 효율성을 위해 다른 언어나 라이브러리를 활용하는 대안도 있습니다. 예를 들어, Python의 datetime 모듈이나 Java의 LocalDate 클래스 등이 있습니다.

## 참고 자료

- Gleam 공식 문서: https://gleam.run/docs/
- Unix 타임스탬프에 대한 정보: https://en.wikipedia.org/wiki/Unix_time
- Python datetime 모듈 도큐먼트: https://docs.python.org/3/library/datetime.html
- Java LocalDate 클래스 도큐먼트: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html