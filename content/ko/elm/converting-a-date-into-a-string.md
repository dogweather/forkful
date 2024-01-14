---
title:    "Elm: 날짜를 문자열로 변환하기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 왜 Elm에서 날짜를 문자열로 변환해야 하는가?

날짜를 문자열로 변환하는 것은 자주 발생하는 문제입니다. 사람들은 날짜를 데이터베이스에서 가져와 사용자에게 보여줄 때 문자열로 변환해야 하며, 다양한 시간대에 맞게 보여주는 등 다양한 상황에서 필요할 수 있습니다. 이를 자동화하는 것은 중요한 일이며, Elm에서 날짜를 문자열로 변환하는 방법을 알아보겠습니다.

## 어떻게 해야 할까요?

우선 날짜의 형식을 결정해야 합니다. 예를 들어, '2021년 8월 23일' 혹은 '8/23/2021'과 같은 형식을 선택할 수 있습니다. 이 예제에서는 'YYYY년 MM월 DD일' 형식을 사용할 것입니다.

먼저, 날짜를 나타내는 `Date` 객체를 생성해야 합니다. 이후 `Date` 객체를 `Intl.DateTimeFormat()`을 사용하여 형식을 설정하고, `DateTime` 모듈의 `toParts` 함수를 사용하여 해당 `Date` 객체를 문자열의 배열로 변환할 수 있습니다. 마지막으로 `String.join` 함수를 사용하여 배열을 하나의 문자열로 결합합니다.

```elm
import DateTime exposing (Date)
import DateTime.Format exposing (toParts)
import String exposing (join)

dateToString : Date -> String
dateToString date =
    let
        formatOptions =
            { year = "numeric", month = "2-digit", day = "2-digit" }

        formattedDate =
            date
                |> DateTime.fromDate
                |> Result.map (Intl.DateTimeFormat "ko-KR" formatOptions)
                |> Result.map toParts
                |> Result.map (List.map .value)
                |> Result.map join " "

    in
    case formattedDate of
        Ok date ->
            date

        Err error ->
            toString error
```

이 함수를 실행하면 다음과 같은 결과를 얻을 수 있습니다.

```
2021년 08월 23일
```

## 더 깊게 들어가보기

날짜를 문자열로 변환하는 것은 간단하지만 실제로는 많은 일이 일어났습니다. `Intl.DateTimeFormat()`은 사용자의 로케일과 시간대를 고려하여 날짜를 형식화하는 기능을 제공합니다. 이를테면 우리나라에서 사용하는 `ko-KR` 로케일을 선택하면 달력 형식과 앞의 예제에서 본 것과 같은 언어 형식을 선택할 수 있습니다.

또한 `DateTime.Format` 모듈의 `toString` 함수를 사용하면 더 간단하게 날짜를 문자열로 변환할 수 있습니다.

```elm
toString : Date -> Int -> Int -> String
```

`toString` 함수에 `Date` 객체와 `고유 밀리초` 및 `UNIX 시간`을 제공하면 날짜를 다양한 형식으로 변환할 수 있습니다.

## 더 알아보기

- [Elm Language 기본 날짜 형식](https://package.elm-lang.org/packages/elm/time/latest/Time#Data-types-and-math)
- [Elm 날짜 형식 빌더 툴](https://package.elm-lang.org/packages/justgook/elm-fmt/latest/String)
- [자바스크립트에서 날짜 형식 지정하기](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)

# 더 알아보기

- [Elm 언어의 `DateTime`모듈](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm 기본 API 문서](https://package.elm-lang.org/packages/elm/core/latest/)
- [Elm ko