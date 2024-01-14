---
title:                "Elm: 미래나 과거의 날짜 계산하기"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 대해 관심이 있을 수 있습니다. 여기서는 함수를 사용하여 어떻게 날짜를 계산할 수 있는지 알아보겠습니다.

## 어떻게

우선 Date 라이브러리를 가져와서 함수를 사용할 준비를 합니다.

```Elm
import Date exposing (..)
```

### 미래의 날짜 계산하기
다음 예제 코드를 보면서 미래의 날짜를 계산하는 방법을 알아보겠습니다.

```Elm
futureDate : Int -> Date -> Date
futureDate days date =
    Date.add days date
```

이 함수는 두 가지 인수를 받아들입니다. 첫번째 인수는 추가할 일 수이고, 두번째 인수는 기준이 되는 날짜입니다. 계산된 결과로 새로운 날짜를 반환합니다.

예를 들어, 오늘 날짜인 2021년 12월 1일에 7일을 더하면 2021년 12월 8일이 됩니다.

```Elm
futureDate 7 (Date.fromDate 2021 12 1)

-- 결과: Date.fromDate 2021 12 8
```

### 과거의 날짜 계산하기

과거의 날짜를 계산하는 방법도 매우 간단합니다. 다음 예제 코드를 보면서 어떻게 할 수 있는지 살펴보겠습니다.

```Elm
pastDate : Int -> Date -> Date
pastDate days date =
    Date.sub days date
```

마찬가지로 두 인수를 받아서 계산된 결과로 새로운 날짜를 반환합니다. 그리고 기준이 되는 날짜에서 일 수를 빼는 것이므로, 더한 예제와는 반대로 과거의 날짜를 계산할 수 있습니다.

예를 들어, 오늘 날짜인 2021년 12월 1일에서 7일을 빼면 2021년 11월 24일이 됩니다.

```Elm
pastDate 7 (Date.fromDate 2021 12 1)

-- 결과: Date.fromDate 2021 11 24
```

## 깊이 파헤치기

미래나 과거의 날짜를 계산하는 방법은 더 많이 있지만, 가장 기본적인 방법을 살펴보았습니다. 그 외에도 Date 라이브러리에는 다양한 함수들이 있으니 꼭 자세히 살펴보시기 바랍니다.

## 관련 자료

- [Elm 공식 문서의 Date 라이브러리 설명](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Elm Date 라이브러리의 소스 코드](https://github.com/elm/time/blob/1.0.0/src/Date.elm)