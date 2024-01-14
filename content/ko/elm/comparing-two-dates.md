---
title:    "Elm: 비교 두 날짜"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# 왜

두 날짜를 비교하는 데 관심이 있는 이유는 무엇일까요? Elm 프로그래밍을 할 때 날짜를 비교하는 것은 중요한 부분입니다. 왜냐하면 위험한 동작이 포함되기 때문입니다. 정확한 방법으로 비교하지 않으면 문제가 발생할 수 있습니다.

# 방법

날짜를 비교하는 방법에는 여러 가지가 있지만 대부분은 잘못된 방법입니다. 그래서 여기서는 올바른 방법과 각 방법이 반환하는 결과를 살펴보겠습니다.

```Elm
-- 예제 1: Date.comparableDates
import Date exposing (Date, comparableDates)

firstDate : Date
firstDate = Date.fromString "2021-01-01"

secondDate : Date
secondDate = Date.fromString "2021-01-31"

comparableDates firstDate secondDate

-- 결과: 0
```

```Elm
-- 예제 2: Date.compare
import Date exposing (Date, compare)

firstDate : Date
firstDate = Date.fromString "2021-01-01"

secondDate : Date
secondDate = Date.fromString "2021-01-31"

compare firstDate secondDate

-- 결과: LT
```

위에서처럼 `comparableDates` 함수와 `compare` 함수는 각각 두 날짜를 비교하여 숫자와 문자열을 반환합니다. 숫자는 날짜 간의 차이를 나타내며, 문자열은 "LT" (firstDate가 secondDate보다 이전), "EQ" (firstDate와 secondDate가 같은 날짜), "GT" (firstDate가 secondDate보다 이후) 중 하나의 값을 갖습니다.

# 딥 다이브

더 깊게 들어가기 전에 이 예제에서 반환되는 숫자나 문자열의 의미를 이해하는 것이 중요합니다. 숫자의 경우, 날짜를 `comparableDates` 함수를 통해 비교하면 오직 하루, 즉 두 날짜 사이의 최소 단위의 차이만 알 수 있습니다. 하지만 `compare` 함수를 사용하면 더 정확한 비교가 가능합니다.

날짜를 비교할 때에는 연도, 월, 일, 시간, 분, 초 등 다양한 요소를 고려해야 합니다. 그리고 어떤 요소를 우선하여 비교할지도 중요합니다. 이런 다양한 상황이 있기 때문에 이론적으로는 무한히 많은 방법으로 날짜를 비교할 수 있습니다. 하지만 Elm에서는 두 가지 함수를 제공하여 더 정확하고 간편하게 날짜를 비교할 수 있도록 해줍니다.

# 이 외에도 알아보기

- [Date 모듈 공식 문서](https://package.elm-lang.org/packages/elm/core/latest/Date)