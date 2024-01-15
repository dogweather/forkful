---
title:                "두 날짜 비교하기"
html_title:           "Elm: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

두 개의 날짜를 비교하는 활동은 우리 일상에서 매우 흔하게 발생합니다. 이러한 비교를 통해 우리는 날짜를 파악하고, 일정을 조정하며, 어떤 일이 언제 시작하고 끝나는지를 쉽게 알 수 있습니다. Elm을 사용하여 날짜를 비교하는 방법을 배우면 일상 생활에서 매우 편리하게 사용할 수 있을 뿐만 아니라, 프로그래밍 실력도 향상시킬 수 있습니다.

## 방법

비교하는 날짜를 나타내는 두 변수 `firstDate`와 `secondDate`을 선언합니다.
그리고 `max` 함수를 사용하여 두 변수를 비교하고 더 큰 값을 반환할 수 있도록 코드를 작성합니다.
이후 `if-then-else` 구문을 사용하여 두 날짜가 같은지를 확인하고, 같을 경우 "두 날짜가 같습니다!"라는 메시지를 출력하도록 합니다. 아래의 예시 코드를 참고하세요.

```Elm
firstDate = 2021-10-07
secondDate = 2021-09-14

max firstDate secondDate

if firstDate == secondDate then
    "두 날짜가 같습니다!"
else
    "두 날짜가 다릅니다!"
```

예시 코드를 실행해보면 `firstDate`가 `2021-10-07`이고 `secondDate`가 `2021-09-14`이기 때문에 "두 날짜가 다릅니다!"라는 메시지가 출력됩니다.

## 깊게 들어가기

Elm에서 두 날짜를 비교할 때에는 `comparison` 모듈을 사용하면 편리합니다. 이 모듈에는 날짜를 비교하는 여러 함수들이 포함되어 있으며, 이를 사용하여 더 복잡한 날짜 비교를 할 수 있습니다. 또한 `Date.fromString` 함수를 사용하여 문자열로 표현된 날짜를 날짜 값으로 변환하는 것도 가능합니다. 아래의 코드를 참고해보세요.

```Elm
import Date
import Comparison exposing (..)

stringDate = "2021-08-27"

dateValue = Date.fromString stringDate

let
  result = compare dateValue Date.today

if result == LT then
    "이번주에 생일파티를 열 예정인가요?"
elsif result == GT then
    "아직 생일까지 시간이 남았군요!"
else
    "생일 축하드립니다!"
```

위의 코드에서는 문자열로 된 날짜를 `Date.fromString`을 사용하여 날짜 값으로 변환하고, `compare` 함수를 사용하여 현재 날짜와 비교하고 있습니다. 그리고 `if-then-else` 구문을 사용하여 비교 결과에 따라 다른 메시지를 출력하고 있습니다.

## 참고

- [Official Elm Guide](https://guide.elm-lang.org/)
- [Comparison 모듈](https://package.elm-lang.org/packages/mgold/elm-date-format/1.0.0/Comparison)
- [Date.fromString 함수](https://package.elm-lang.org/packages/elm-lang/core/latest/Date#fromString)

----
# 참고 자료

앞서 포함된 모든 링크를 참고하시면서 예제 코드를 따라해보세요. 그리고 더 복잡한 날짜 비교에 도전해보세요!