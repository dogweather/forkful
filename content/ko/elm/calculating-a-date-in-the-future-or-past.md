---
title:    "Elm: 미래나 과거 날짜 계산하기"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# 왜

우리는 때때로 미래나 과거의 특정 날짜를 계산하는 일이 필요할 수 있습니다. 예를 들어, 언제 예정된 프로젝트가 끝나는지, 혹은 다음 주 금요일 휴가를 낼 수 있는지 등등 다양한 이유로 필요할 수 있습니다. Elm은 간단한 코드로 이러한 날짜 계산을 할 수 있게 해줍니다.

## 해결방법

먼저, 날짜를 계산하는 데에는 [DateTime](https://package.elm-lang.org/packages/elm/time/latest/Time#DateTime) 모듈을 사용합니다. 이 모듈은 기본적인 날짜 계산을 위한 함수들을 제공해줍니다.

```
import Time exposing (..)
import Time.Date exposing (..)

today : Date
today =
    Time.now
        |> Time.toUtc
        |> Time.toYearMonthDay
        |> Date.fromYearMonthDay
```

위 예제 코드는 오늘 날짜를 구하는 코드입니다. [Time.toYearMonthDay](https://package.elm-lang.org/packages/elm/time/latest/Time#toYearMonthDay) 함수를 사용하여 현재 시간을 연, 월, 일로 변환한 후 [Date.fromYearMonthDay](https://package.elm-lang.org/packages/elm/time/latest/Time-Date#fromYearMonthDay) 함수를 사용하여 [Date](https://package.elm-lang.org/packages/elm/time/latest/Time-Date#Date) 타입으로 변환합니다.

Date 타입은 [OffsetDate](https://package.elm-lang.org/packages/elm/time/latest/Time-Date#OffsetDate)와 [ZoneDate](https://package.elm-lang.org/packages/elm/time/latest/Time-Date#ZoneDate) 두 가지가 있습니다. OffsetDate는 우리가 일반적으로 사용하는 타임존에 맞춘 날짜를 표현하고, ZoneDate는 UTC 기준의 날짜를 표현합니다.

아래 코드는 오늘 부터 7일 후의 날짜를 구하는 예제입니다.

```
import Time exposing (..)
import Time.Date exposing (..)

nextWeek : Date
nextWeek =
    today
        |> toIsoString
        |> Time.fromIsoString
        |> Time.add (Time.weeks 1)
        |> Time.toYearMonthDay
        |> Date.fromYearMonthDay
```

첫 줄에서 현재 날짜를 [ISO 8601](https://ko.wikipedia.org/wiki/ISO_8601) 형태의 문자열로 변환합니다. 이후 [Time.fromIsoString](https://package.elm-lang.org/packages/elm/time/latest/Time#fromIsoString) 함수를 사용하여 문자열을 [Posix](https://en.wikipedia.org/wiki/POSIX) 시간으로 변환합니다. 이어서 [Time.add](https://package.elm-lang.org/packages/elm/time/latest/Time#add) 함수를 사용하여 7일을 더하고, 다시 연, 월, 일 형태로 변환하여 Date 타입으로 변환합니다.

## 더 깊은 이해

더 복잡한 날짜 계산을 위해서는 [Posix](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix)와 [Time.Zone](https://package.elm-lang.org/packages/elm/time/latest/Time-Zone) 모듈을 사용할 수 있습니다. Posix는 시간을 나타내는 가장 기본적인 타입이고, Time.Zone은 타임존을 나타내는 타입입니다. Date와는 달리 Posix와 Time.Zone은 모두 [ISO 8601](https://ko.wikipedia.org/wiki/ISO_8601) 형식을 따르는 문자열로 변환되어 전달되어야 합니다.

더 자세한 내용은 [공식 문서](https://package.elm-lang.org/packages/elm/time/latest/Time)를 참고하시기 바랍니다.

# 참고 자료

- [Elm 공식