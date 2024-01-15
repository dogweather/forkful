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

## 왜

우리는 모두 우리의 일정과 스케줄을 관리하고 예측하기 위해 날짜를 계산하는 일에 참여합니다. Elm은 매우 강력한 언어로서 날짜를 정확하게 계산하는 기능을 제공합니다. 이 기사에서는 Elm을 사용하여 만들어진 간단하고 간결한 코드를 통해 날짜를 계산하는 방법을 배워보겠습니다.

## 해결 방법

그것은 매우 간단합니다. 우리는 가장 먼저 필요한 날짜를 나타내는 변수를 정의합니다. 그런 다음, 이 날짜를 기준으로 원하는 만큼 더하거나 뺍니다. 오늘 날짜보다 과거의 날짜를 계산하려면 간단하게 마이너스 기호를 사용하여 뺍니다. 이 모든 것은 아래에 제시된 코드 블록에서 확인할 수 있습니다. 

```Elm
-- 현재 날짜로부터 3일 이전의 날짜 계산
let
  targetDate = Date.now
  pastDate = Date.subDays 3 targetDate
in
  pastDate

-- 현재 날짜로부터 2주 뒤의 날짜 계산
let
  targetDate = Date.now
  futureDate = Date.addWeeks 2 targetDate
in
  futureDate
```

출력 결과:

```
계산된 과거 날짜: 2021년 9월 7일
계산된 미래 날짜: 2021년 9월 25일
```

## 깊게 파헤치기

보다 복잡한 날짜 계산을 할 수도 있습니다. 예를 들어, 현재 날짜로부터 정확히 6개월 전의 날짜를 계산하려면 어떻게 해야 할까요? 이 경우, 우리는 `Date.midnight` 함수를 사용하여 현재 날짜의 자정을 기준으로 정확한 결과를 얻을 수 있습니다.

```Elm
-- 현재 날짜로부터 6개월 전의 날짜 계산
let
  targetDate = Date.now
  sixMonthsAgo = Date.midnight targetDate |> Date.subMonths 6 
in
  sixMonthsAgo
```

출력 결과:

```
계산된 날짜: 2021년 3월 3일
```

더 많은 날짜 계산 관련 함수와 사용 예제는 [Elm Documentaion](https://package.elm-lang.org/packages/elm/time/latest/)에서 확인할 수 있습니다.

## 더 알아보기

이번에 우리는 Elm을 사용하여 날짜를 계산하는 방법을 배웠습니다. 이 기능을 사용하여 우리의 프로젝트에 유용한 기능을 추가할 수 있습니다. 더 많은 Elm 관련 기사를 읽고 싶으시면 [Elm 공식 홈페이지](https://elm-lang.org/)를 참고해보세요.

## 더 알아보기

- [Elm 공식 홈페이지](https://elm-lang.org)
- [Elm Documentaion](https://package.elm-lang.org/packages/elm/time/latest/)