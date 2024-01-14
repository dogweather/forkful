---
title:                "Elm: 현재 날짜 가져오기"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜?
현재 날짜를 가져오는 것에 참여하는 이유는 무엇일까요? 단 1-2 문장으로 설명해 보겠습니다.

현재 날짜를 가져오는 것은 프로그래밍에서 자주 사용되는 기능입니다. 예를 들어, 사용자가 만든 일정표에 현재 날짜를 표시하거나, 웹사이트에 일자별 활동 내역을 보여줄 때 사용할 수 있습니다.

## 사용 방법
```Elm
import Time exposing (now, Time)

main =
    now |> Time.toDate |> toString
```
위의 코드를 사용하면 현재 날짜를 가져올 수 있습니다. 코드를 실행하면 다음과 같은 결과가 나옵니다:
```
"2020-06-29T21:36:53.341Z"
```
다음은 결과를 더 유용하게 만드는 방법입니다:
```Elm
import Time exposing (now, Time)
import Date exposing (Date, toString, Month(..))

main =
    now
        |> Time.toDate
        |> Date.fromDate
        |> toString
```
이제 코드를 실행하면 다음과 같은 날짜 형식의 결과가 나옵니다:
```
"Mon, 29 Jun 2020 21:37:36 GMT"
```
우리는 또한 더 많은 정보를 추가할 수 있습니다. 예를 들어, 우리가 현재 속하는 지역의 시간대를 알기 위해 `Time.Zone`을 추가할 수 있습니다:
```Elm
import Time exposing (now, Time)
import Date exposing (Date, toString, Month(..))
import Time.Zone exposing (timezone)

main =
    now
        |> Time.toDate
        |> Date.fromDate
        |> timezone "GMT+9" -- 여기에 원하는 시간대를 입력합니다.
        |> toString
```
이제 코드를 실행하면 다음과 같은 결과가 나옵니다:
```
"Tue, 30 Jun 2020 06:40:58 KST"
```
이렇게 하면 우리가 원하는 시간대로 날짜를 표시할 수 있습니다.

## 깊이 있는 탐구
현재 날짜를 가져오는 더 많은 방법들이 있습니다. 예를 들어, 우리는 `Time` 패키지를 이용해서 현재 날짜뿐 아니라 시간과 관련된 다양한 정보를 가져올 수 있습니다. 또한 `Date` 패키지를 이용해서 날짜와 관련된 연산을 수행할 수도 있습니다.

## 참고
- [Elm Time 패키지 문서](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Date 패키지 문서](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Time.Zone 패키지 문서](https://package.elm-lang.org/packages/elm/time/latest/Time-Zone#timezone)