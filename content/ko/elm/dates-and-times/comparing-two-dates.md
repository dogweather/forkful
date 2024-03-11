---
date: 2024-01-20 17:32:45.813411-07:00
description: "\uB450 \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD558\uB294 \uAC83\uC740 \uADF8\
  \uB4E4 \uC0AC\uC774\uC758 \uCC28\uC774\uB97C \uACC4\uC0B0\uD558\uAC70\uB098 \uC21C\
  \uC11C\uB97C \uACB0\uC815\uD558\uB294 \uD504\uB85C\uC138\uC2A4\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC720\uD6A8\uC131 \uAC80\uC0AC, \uAE30\uAC04\
  \ \uACC4\uC0B0, \uC774\uBCA4\uD2B8 \uC608\uC815\uACFC \uAC19\uC740 \uB2E4\uC591\uD55C\
  \ \uC774\uC720\uB85C \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:29.047481-06:00'
model: gpt-4-1106-preview
summary: "\uB450 \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD558\uB294 \uAC83\uC740 \uADF8\uB4E4\
  \ \uC0AC\uC774\uC758 \uCC28\uC774\uB97C \uACC4\uC0B0\uD558\uAC70\uB098 \uC21C\uC11C\
  \uB97C \uACB0\uC815\uD558\uB294 \uD504\uB85C\uC138\uC2A4\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB294 \uC720\uD6A8\uC131 \uAC80\uC0AC, \uAE30\uAC04 \uACC4\
  \uC0B0, \uC774\uBCA4\uD2B8 \uC608\uC815\uACFC \uAC19\uC740 \uB2E4\uC591\uD55C \uC774\
  \uC720\uB85C \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD569\uB2C8\uB2E4."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
두 날짜를 비교하는 것은 그들 사이의 차이를 계산하거나 순서를 결정하는 프로세스입니다. 프로그래머는 유효성 검사, 기간 계산, 이벤트 예정과 같은 다양한 이유로 날짜를 비교합니다.

## How to: (어떻게 하나요?)
```Elm
import Time exposing (Posix)
import Date

-- 두 Posix 시간을 비교하는 함수
compareDates : Posix -> Posix -> Order
compareDates date1 date2 =
    if date1 == date2 then
        EQ
    else if date1 < date2 then
        LT
    else
        GT

-- 날짜 비교 예제
date1 : Posix
date1 =
    Time.millisToPosix 1500000000000

date2 : Posix
date2 =
    Time.millisToPosix 1600000000000

-- compareDates 함수 사용 예
result : String
result =
    case compareDates date1 date2 of
        EQ ->
            "두 날짜는 동일합니다."
        LT ->
            "첫 번째 날짜가 더 이릅니다."
        GT ->
            "첫 번째 날짜가 더 늦습니다."

-- 출력: 두 날짜나 동일하지 않음: 첫 번째 날짜가 더 이릅니다.
```

## Deep Dive (심층 분석)
Elm에서 날짜 비교는 `Time.Posix` 타입을 통해 이루어집니다, 이는 1970년 1월 1일 자정 UTC 이후 밀리초로 표현됩니다. `Time.millisToPosix` 함수는 밀리초를 `Posix` 타입으로 변환합니다. `compareDates` 함수는 두 `Posix` 값을 비교해 크기를 `Order` 타입 (`LT`, `GT`, `EQ`)으로 반환합니다.

과거 JavaScript 등 다른 언어는 날짜를 직접적으로 비교하는 방식을 제공했습니다만, Elm은 더 엄격하게 타입을 정의하고 순수 함수를 사용하여 날짜를 비교합니다. 여러 날짜 라이브러리가 있지만 기본적인 비교는 `Time` 모듈로 충분합니다.

## See Also (함께 보기)
- Elm: [Time 모듈](https://package.elm-lang.org/packages/elm/time/latest/)
- Elm: [Date 라이브러리의 일부 함수](https://package.elm-lang.org/packages/justinmimbs/date/latest/)
- 좋은 참고 자료: [Elm-lang 공식 가이드의 Time 관련 내용](https://guide.elm-lang.org/effects/time.html)
