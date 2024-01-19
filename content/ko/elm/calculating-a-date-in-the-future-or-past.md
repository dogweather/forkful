---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "Elm: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

미래 또는 과거의 날짜를 계산하는 것은 특정 날짜에 일정 기간을 더하거나 뺀 날짜를 찾는 과정입니다. 이것은 프로그램에서 이벤트의 시간을 추적하거나 예약 기능을 구현하는데 상당히 많이 사용됩니다.

## 어떻게?

```Elm
import Time
import Time.Extra

calculateFutureDate : Time.Posix -> Time.Zone -> Int -> Time.Posix
calculateFutureDate startDate zone daysToAdd =
    let
        duration =
            Time.Extra.daysToMillis daysToAdd
    in
        Time.millisSinceEpoch startDate + duration 

main = 
    let 
        zone = Time.here
        startDate = Time.millisSinceEpoch 1631147523
        daysToAdd = 30
    in 
        calculateFutureDate startDate zone daysToAdd
```
이 코드는 Elm의 `Time`과 'Time.Extra' 모듈을 이용하여 현재의 시간에 30일을 더하는 일을 합니다.

## 깊이 들어가 보기

미래 또는 과거의 날짜 계산은 프로그래밍에서 점점 중요해진 개념입니다. 이것이 이렇게 중요해진 이유는 웹 애플리케이션에서 기능상의 이유로이며, 사용자에게 예정된 이벤트나 마감일을 알려줄 수 있습니다.

이러한 작업을 수행하는 데는 몇 가지 대안이 있습니다. 일부 언어는 별도의 날짜 계산 라이브러리를 제공합니다. Elm에서는 `Time` 모듈과 'Time.Extra'이 제공합니다.

이 구현에서는 밀리초로 변환하여 날짜에 일수를 더했습니다. 이 방법은 시간대를 고려하지 않으므로 시간대가 중요한 애플리케이션에서는 다른 방법을 사용해야 할 수 있습니다.

## 참고하기

Time.Extra: [https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/](https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/) 

Asking and Answering Questions about Time: [https://elmprogramming.com/time.html](https://elmprogramming.com/time.html)