---
date: 2024-01-20 17:32:45.813411-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Elm\uC5D0\uC11C \uB0A0\
  \uC9DC \uBE44\uAD50\uB294 `Time.Posix` \uD0C0\uC785\uC744 \uD1B5\uD574 \uC774\uB8E8\
  \uC5B4\uC9D1\uB2C8\uB2E4, \uC774\uB294 1970\uB144 1\uC6D4 1\uC77C \uC790\uC815 UTC\
  \ \uC774\uD6C4 \uBC00\uB9AC\uCD08\uB85C \uD45C\uD604\uB429\uB2C8\uB2E4. `Time.millisToPosix`\
  \ \uD568\uC218\uB294 \uBC00\uB9AC\uCD08\uB97C `Posix` \uD0C0\uC785\uC73C\uB85C \uBCC0\
  \uD658\uD569\uB2C8\uB2E4.\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.489812-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Elm\uC5D0\uC11C \uB0A0\uC9DC \uBE44\
  \uAD50\uB294 `Time.Posix` \uD0C0\uC785\uC744 \uD1B5\uD574 \uC774\uB8E8\uC5B4\uC9D1\
  \uB2C8\uB2E4, \uC774\uB294 1970\uB144 1\uC6D4 1\uC77C \uC790\uC815 UTC \uC774\uD6C4\
  \ \uBC00\uB9AC\uCD08\uB85C \uD45C\uD604\uB429\uB2C8\uB2E4."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
weight: 27
---

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
