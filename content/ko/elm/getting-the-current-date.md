---
title:    "Elm: 현재 날짜 가져오기"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 왜

현재 날짜를 얻는 일은 우리의 일상에서 매우 중요합니다. 예를 들어, 우리는 만료 날짜를 확인하는 온라인 쇼핑을 할 때 우리가 사는 지역의 미국 시간대를 알아야합니다. 따라서 다음 섹션에서는 Elm을 사용하여 현재 날짜를 가져오는 방법에 대해 알아보겠습니다.

## 어떻게

가장 간단한 방법은 `Date.now`를 사용하는 것입니다. 우선, 모듈을 가져오기 위해 `Date`를 선언하고, `now` 함수를 사용하여 현재 날짜를 얻을 수 있습니다.

```Elm
import Date exposing (Date)

date : Date
date = Date.now
```

위의 예제에서 `date` 변수는 현재 날짜를 나타냅니다. 또한 `Date.now` 함수는 현재 날짜를 밀리초로 반환합니다. 따라서 다음과 같이 계산하여 우리가 원하는 형식으로 날짜를 얻을 수 있습니다.

```Elm
date : Date
date =
    Date.fromTime (Date.now / 1000 / 1000 / 60 / 60 / 24)
```

위의 예제에서 `Date.fromTime` 함수는 날짜를 나타내는 `Date` 유형을 반환합니다. 이제 `date` 변수에는 현재 날짜를 나타내는 `Date` 유형이 할당됩니다.

## 딥 다이브

Elm에서 날짜를 다루는 깊은 내용을 알고 싶다면, `Time` 모듈을 살펴보는 것이 좋습니다. 이 모듈에는 날짜와 관련된 다양한 기능들이 포함되어 있습니다. 예를 들어, `Time.month` 함수를 사용하면 현재 날짜의 월을 가져올 수 있습니다.

```Elm
import Time exposing (month)

currentMonth : Int
currentMonth =
    Time.month (Time.millisToPosix (Date.now / 1000 / 1000))
```

위의 예제에서 `Time.millisToPosix` 함수를 사용하여 밀리초를 POSIX 시간으로 변환하고, `Time.month` 함수를 사용하여 현재 월을 얻습니다. 또한 `Time` 모듈에는 날짜를 포맷하는 유용한 기능들이 있으므로, 꼭 살펴보시기 바랍니다.

## 또한 보기

- [Elm 공식 문서 - Date 모듈](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Date 모듈 예제 코드](https://gist.github.com/ninjaPixel/da08cca95ff7c9440d50)