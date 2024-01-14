---
title:                "Elm: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 왜 Elm 프로그래밍을 배워야 할까요?

하지만 새로운 기술을 배우는 것은 절대 쉬운 일이 아닙니다. 그래서 이번 포스트에서는 Elm 프로그래밍 언어를 공부하는 이유를 알려드리고자 합니다. Elm은 시대를 앞서가는 함수형 프로그래밍 언어로, 견고한 구조와 쉬운 문법을 가지고 있으며, 웹 애플리케이션을 개발하는 데 매우 효과적입니다.

## 어떻게 다른 날짜를 문자열로 변환할 수 있을까요?

```Elm
import Time exposing (..)

dateToString : Date -> String
dateToString date =
    case toMonthAndDay date of
        Just (month, day) ->
            let
                monthStr =
                    if month < 10 then
                        "0" ++ toString month
                    else
                        toString month
            in
            monthStr ++ "/" ++ toString day

        Nothing ->
            "날짜를 찾을 수 없습니다."
```

위의 예제에서는 Elm의 `Time` 라이브러리를 이용해 날짜를 문자열로 변환하는 방법을 보여줍니다. `toMonthAndDay` 함수를 통해 월과 일을 추출하고, 이를 문자열로 처리하여 최종적으로 원하는 포맷으로 변환합니다.

출력 예시:
```
dateToString (fromCalendarDate 2021 1 1)
```
```
"01/01"
```

## 깊게 들어가보면?

날짜를 문자열로 변환하는 것은 꽤 간단한 작업처럼 보이지만, 실제로는 많은 고민과 계획이 필요한 작업입니다. 왜냐하면 다양한 언어와 문화, 지역마다 날짜를 표현하는 방식이 다를 수 있기 때문입니다. 때문에 Elm은 `Time` 라이브러리를 통해 이를 처리할 수 있는 다양한 함수들을 제공합니다.

`dateToString` 함수에서도 `toMonthAndDay`, `fromCalendarDate` 외에도 `toMinuteAndSecond`을 이용해서 시간까지 포함시킬 수 있습니다. 또한 `Date` 타입에서 추출한 정보를 `toString` 함수를 통해 원하는 포맷으로 자유롭게 조합할 수 있습니다.

# 더 알아보기

위 예제에서 사용된 `Time` 라이브러리의 다양한 함수들에 대해 더 자세히 알아보고 싶다면 아래 링크를 참고해주세요.

[Time - Elm 문서](https://elm-lang.org/docs/time)