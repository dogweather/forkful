---
title:                "Elm: 미래나 과거에서의 날짜 계산"
simple_title:         "미래나 과거에서의 날짜 계산"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

미래나 과거 날짜를 계산하는 것에 참여할 이유는 모든 프로그래머에게 중요한 기능입니다. 이를테면, 캘린더 앱이나 예약 시스템에서 필수적인 요소입니다.

## 방법

먼저, 우리는 `elm/time` 라이브러리를 사용해 날짜를 계산하는 함수를 불러올 것입니다. 이 함수는 현재 시간에서 지난 날짜나 미래 날짜로의 이동을 지원합니다.

```Elm
import Time exposing (..)

today : Date
today =
    Time.today

pastDate : Date
pastDate =
    Time.subtract 2 Days today

futureDate : Date
futureDate =
    Time.add 1 Month today
```

위의 예시 코드에서 우리는 `today` 변수를 현재 날짜로 설정하고, 이를 이용해 `pastDate` 변수를 2일 전으로 설정하는 방법과 `futureDate` 변수를 1달 후로 설정하는 방법을 보여주고 있습니다. 이제 우리는 `pastDate`와 `futureDate`를 출력해보면,

```Elm

-- Output:
July 28, 2020
October 19, 2020

```

위와 같은 결과를 볼 수 있습니다.

## 심층 탐구

이미 보셨듯이, `Time` 모듈에는 우리가 필요한 다양한 함수들이 존재합니다. 이를 활용하면, 더 복잡한 날짜 계산도 가능해집니다. 예를 들어, 현재 시간에서 1년 후의 마지막 날짜를 계산하거나, 혹은 현재 시간과 시계를 보는 사이의 시간차를 계산할 수도 있습니다. 더 자세한 사용 방법은 `Time` 모듈의 공식 문서를 참고하시기 바랍니다.

## 더 보기

- [Elm 공식 문서](https://guide.elm-lang.org/)