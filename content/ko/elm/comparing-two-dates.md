---
title:                "Elm: 두 날짜를 비교하는 방법"
simple_title:         "두 날짜를 비교하는 방법"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
두 날짜를 비교하는 것에 대한 이유는 무엇일까요? 이 글에서는 Elm 프로그래밍 언어를 사용해 두 날짜를 비교하는 방법에 대해 알아보겠습니다.

## 어떻게
Elm 프로그래밍 언어에서 두 날짜를 비교하는 방법은 간단합니다. 먼저, 비교하고 싶은 날짜를 `Date` 타입으로 변환합니다. 그리고 두 날짜를 `compare` 함수를 사용해 비교하면 됩니다. 아래는 예시 코드와 결과입니다.

```Elm
-- 두 날짜 비교하기

-- 비교하고 싶은 날짜
date1 : Date
date1 =
    fromCalendarDate 2020 9 5

date2 : Date
date2 = 
    fromCalendarDate 2021 5 20

-- 두 날짜를 비교
comparison : Ordering
comparison =
    compare date1 date2

-- 출력
comparison == LT  -- date1 < date2
```

### 출력
출력 결과로는 `date1`이 `date2`보다 먼저 일어난 날짜이기 때문에 `LT`가 나오게 됩니다.

## 깊게 파보기
`compare` 함수를 사용해 두 날짜를 비교할 때, 어떤 일이 일어나는지 조금 더 자세히 알아보겠습니다. 우선, `compare` 함수는 `Ordering` 타입을 리턴합니다. 이는 `LT`, `EQ`, `GT` 중 하나가 될 수 있습니다. `LT`는 첫 번째 인자가 두 번째 인자보다 작은 경우를, `EQ`는 두 인자가 같은 경우를, `GT`는 첫 번째 인자가 두 번째 인자보다 큰 경우를 나타냅니다.

`Ordering`은 `comparable`이라는 타입 클래스의 인스턴스입니다. 이는 비교 가능한 타입을 위한 용도로 사용됩니다. 우리는 `Ordering` 타입을 `compare` 함수의 결과로 사용해서 비교하고 싶은 객체의 크기를 비교할 수 있습니다.

## 관련 글
- [Elm 공식 문서 - Date 모듈](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Elm 공식 문서 - Date 모듈의 compare 함수](https://package.elm-lang.org/packages/elm/time/latest/Time#compare) 
- [JavaScript에서의 날짜 비교 방법](https://www.geeksforgeeks.org/how-to-compare-date-in-javascript/) 

## 참고
이번 글에서는 Elm 프로그래밍 언어를 사용해 두 날짜를 비교하는 방법에 대해 알아보았습니다. `compare` 함수를 사용해 두 날짜를 비교하고, `Ordering` 타입을 통해 비교 결과를 확인할 수 있습니다. 비교 가능한 타입에 대해 더 알고 싶다면 `comparable` 타입 클래스를 참고하시기 바랍니다.