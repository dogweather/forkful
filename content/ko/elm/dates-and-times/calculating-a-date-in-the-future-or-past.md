---
date: 2024-01-20 17:31:06.353693-07:00
description: "\uBBF8\uB798 \uB610\uB294 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\
  \uC740 \uD2B9\uC815 \uAE30\uAC04\uC744 \uB354\uD558\uAC70\uB098 \uBE7C\uC11C \uC0C8\
  \uB85C\uC6B4 \uB0A0\uC9DC\uB97C \uC5BB\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC608\uC57D \uC2DC\uC2A4\uD15C, \uD560\uC778 \uCFE0\
  \uD3F0\uC758 \uB9CC\uB8CC\uC77C \uACC4\uC0B0 \uB4F1 \uB2E4\uC591\uD55C \uAE30\uB2A5\
  \uC744 \uAD6C\uD604\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.130395-06:00'
model: gpt-4-1106-preview
summary: "\uBBF8\uB798 \uB610\uB294 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uC740\
  \ \uD2B9\uC815 \uAE30\uAC04\uC744 \uB354\uD558\uAC70\uB098 \uBE7C\uC11C \uC0C8\uB85C\
  \uC6B4 \uB0A0\uC9DC\uB97C \uC5BB\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uC608\uC57D \uC2DC\uC2A4\uD15C, \uD560\uC778 \uCFE0\uD3F0\
  \uC758 \uB9CC\uB8CC\uC77C \uACC4\uC0B0 \uB4F1 \uB2E4\uC591\uD55C \uAE30\uB2A5\uC744\
  \ \uAD6C\uD604\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
미래 또는 과거의 날짜 계산은 특정 기간을 더하거나 빼서 새로운 날짜를 얻는 것입니다. 프로그래머들은 예약 시스템, 할인 쿠폰의 만료일 계산 등 다양한 기능을 구현하기 위해 이를 사용합니다.

## 사용 방법:
Elm에서는 `elm/time` 패키지를 사용하여 날짜를 더하고 빼보겠습니다.

```Elm
import Time
import Time.Extra exposing (..)
import Date

-- 날짜 생성하기
dateNow : Date.Date
dateNow = Date.fromPosix (Time.millisToPosix 1580550000000)

-- 미래 날짜 계산하기
addDaysToNow : Int -> Date.Date
addDaysToNow daysToAdd =
    Date.add Days daysToAdd dateNow

-- 과거 날짜 계산하기
subtractDaysFromNow : Int -> Date.Date
subtractDaysFromNow daysToSubtract =
    Date.sub Days daysToSubtract dateNow

-- 예제 출력
exampleFutureDate : Date.Date
exampleFutureDate = addDaysToNow 10

examplePastDate : Date.Date
examplePastDate = subtractDaysFromNow 10
```

`addDaysToNow` 함수는 현재 날짜에 일 수를 더해 새 날짜를, `subtractDaysFromNow` 함수는 뺀 날짜를 반환합니다.

## 심화 정보:
과거에는 Elm에서 `elm-lang/core`의 `Time` 모듈로 날짜를 처리했습니다. 하지만 현재는 `elm/time` 패키지가 이를 대체하고 더 풍부한 API를 제공합니다.

또한, JavaScript의 `Date` 객체로 작업하거나 `moment.js` 같은 라이브러리를 사용하는 것과 달리, Elm에서는 순수 함수적 접근을 사용해 시간과 날짜를 다루게 됩니다. 이는 부수효과를 최소화하고 프로그램의 예측 가능성을 높이는 데 도움이 됩니다.

`Date` 모듈의 경우 Elm 표준 라이브러리에 내장되어 있지 않기 때문에, `elm-time` 라이브러리를 추가해야 합니다. 이 라이브러리는 실제로는 POSIX 시간을 기반으로 일, 주, 월, 년을 더하거나 빼는 연산을 제공합니다.

## 관련 정보:
- Elm Time 패키지: [package.elm-lang.org/packages/elm/time/latest](https://package.elm-lang.org/packages/elm/time/latest)
- Elm Discuss (for community help): [discourse.elm-lang.org](https://discourse.elm-lang.org)
