---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Elm: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇인가요? 그리고 왜 필요할까요?

날짜를 미래나 과거로 계산하는 것은 프로그래밍에서 중요한 작업입니다. 이를 통해 특정 날짜의 이벤트를 계획하거나 미래에 발생할 이벤트를 예측할 수 있습니다. 프로그래머들은 이를 수행하는 이유는 앞으로 발생할 사건들에 대한 정확한 계획과 예측을 가능하게 하기 위해서입니다.

## 하는 방법:

```Elm
import Time

Time.add Time.day 3 Time.Jan 10  // 2020년 1월 13일을 계산합니다.
Time.sub Time.hour 6 Time.Jan 10  // 2020년 1월 9일 18시를 계산합니다.
```

## 깊이 파헤치기:

(1) 과거부터 현재까지 계산의 역사적 배경은 고대 로마 시대까지 거슬러 올라갑니다. 그 당시에는 검은 식탁과 달력의 계산을 통해 날짜를 파악했습니다.

(2) 날짜를 계산하는 또 다른 방법으로는 moment.js라는 자바스크립트 라이브러리를 사용하는 것입니다. elm-community/elm-time이 제공하는 Time 모듈은 기본적인 날짜 계산 기능을 제공하지만 moment.js 보다는 적은 기능을 제공합니다.

(3) Elm에서는 Time 모듈 내에서 날짜를 계산하는 함수를 제공합니다. 이 함수들은 내부적으로는 밀리초를 처리하기 때문에 시작 날짜와 offset 값을 계산하여 결과값을 반환해주는 방식으로 작동합니다.

## 더 알아보기:

- [Moment.js](https://momentjs.com/): 자바스크립트 라이브러리
- [elm-community/elm-time](https://package.elm-lang.org/packages/elm-community/elm-time/latest/): Elm에서 제공하는 Time 모듈