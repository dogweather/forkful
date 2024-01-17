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

#시작하기 

본격적인 프로그래밍을 시작하기 전에 우리는 자주 쓰이는 기능 중 하나를 배워야 합니다 - 날짜 비교입니다. 날짜 비교는 두 날짜가 언제인지를 비교하는 것을 말합니다. 프로그래머들이 날짜 비교를 하는 이유는 매우 간단합니다. 우리는 일상 생활에서 날짜를 자주 다루게 되는데, 날짜를 비교하는 것은 우리의 코드를 더욱 유연하고 정확하게 만들어 줍니다. 그래서 오늘은 Elm에서 두 날짜를 비교하는 방법을 배워보겠습니다.

## 무엇 & 왜?

날짜 비교는 두 날짜가 언제인지를 비교하는 것을 말합니다. 그 이유는 우리가 날짜를 비교해야 하는 경우가 매우 많기 때문입니다. 예를 들어, 우리가 주문한 상품이 언제 배송될 지를 알고 싶을 때, 배송일이 지난 상품을 자동으로 취소하고 싶을 때 등등 많은 상황에서 우리는 두 날짜를 비교하게 됩니다.

## 어떻게:

Elm에서는 날짜를 비교하려면 `Date` 라는 모듈을 사용해야 합니다. 두 날짜를 비교하려면 `Date.compare ` 함수를 사용하면 됩니다. 아래는 Elm에서 두 날짜를 비교하는 예제 코드입니다.

```Elm
import Date

today = Date.fromCalendarDate 2020 9 9
tomorrow = Date.fromCalendarDate 2020 9 10

Date.compare today tomorrow
```

위 코드를 실행하면 `LT` 라는 출력값을 얻을 수 있습니다. 이 값은 "less than"의 약어로, 첫 번째 날짜가 두 번째 날짜보다 이전임을 의미합니다. 만약 두 날짜가 같다면 `EQ`를, 첫 번째 날짜가 두 번째 날짜보다 나중이라면 `GT`를 얻게 됩니다.

## 깊이 들어가기:

Elm에서는 날짜를 비교할 때 `Date.compare` 함수를 사용하지만, 다른 언어에서는 다른 방식으로 날짜 비교를 할 수도 있습니다. 예를 들어 JavaScript에서는 `Date` 객체의 `getTime` 메서드를 사용하여 두 날짜를 밀리초 단위로 변환한 뒤 비교합니다. 따라서 어떤 언어를 사용하던지, 날짜 비교를 할 때는 해당 언어에서 제공하는 기능을 찾아서 사용하시면 됩니다.

## 관련 자료:

- Elm 공식 문서: https://elm-lang.org/docs/date#compare