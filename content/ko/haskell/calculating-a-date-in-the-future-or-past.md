---
title:                "Haskell: 미래나 과거에서 날짜 계산하기"
simple_title:         "미래나 과거에서 날짜 계산하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 관심이 있는 이유는 무엇일까요? 아마도 생일이나 어떤 특정 날짜가 무슨 요일이었는지 알고 싶은 경우일 것입니다. 미래 날짜를 계산하면 예약된 이벤트나 휴가를 계획할 때 유용할 것입니다. 이번 블로그 포스트에서는 하스켈을 사용하여 간단한 날짜 계산 기능을 구현하는 방법을 알아보겠습니다.

## 어떻게

우선 하스켈에서 날짜를 나타내는 방법은 `Day` 데이터 타입을 사용하는 것입니다. 이 데이터 타입은 연, 월, 일을 나타내는 `Date` 타입과 매핑됩니다.

```Haskell
data Day = Date Year Month DayOfMonth
```

이제 날짜를 나타내는 `Day` 데이터 타입을 활용하여 간단한 함수를 만들어보겠습니다. 여기서는 미래 날짜를 계산하는 함수 `calculateFutureDate`를 만들어보겠습니다. 이 함수는 현재 날짜와 미래로 계산하고 싶은 일수를 입력받아 해당 일수가 더해진 미래 날짜를 반환합니다.

```Haskell
calculateFutureDate :: Day -> Int -> Day
calculateFutureDate (Date year month day) days =
  let
    totalDays = (toModifiedJulianDay (fromGregorian year month day)) + fromIntegral days
    (newYear, newMonth, newDay) = toGregorian (ModifiedJulianDay totalDays)
  in Date newYear newMonth newDay
```

위 코드에서 우리가 사용한 함수는 `toModifiedJulianDay`와 `fromGregorian` 그리고 `toGregorian`입니다. 이 함수들은 하스켈의 `time` 라이브러리에서 제공하는 함수로서, 날짜를 다루는 다양한 함수들이 있습니다. 이를 사용하여 간단하게 날짜를 계산할 수 있습니다.

그러나 이 예제에서는 날짜 계산 로직보다는 함수를 만드는 방법에 대한 예제로서 날짜 계산을 사용하였으므로, 실제 적용할 때는 더 다양한 조건을 고려하여 로직을 완성해야합니다.

## 딥 다이브

날짜 계산을 위해 사용한 `toModifiedJulianDay` 함수는 현재 날짜를 Modified Julian Day 형식으로 변환합니다. 이 형식은 다양한 날짜를 숫자로 표현하는 방식으로, 날짜 계산에 유용하게 사용할 수 있습니다.

따라서 우리는 `toModifiedJulianDay`와 `fromGregorian` 함수를 이용하여 현재 날짜를 Modified Julian Day 형식으로 변환하고, 더해야할 날짜를 해당 숫자만큼 더한 후 다시 `toGregorian` 함수를 이용하여 날짜 형식으로 변환하여 미래 날짜를 구할 수 있습니다.

하지만 더 복잡한 날짜 계산을 위해서는 더 많은 함수를 사용해야할 수 있으며, 이에 대한 예제는 하스켈의 `time` 라이브러리 문서를 참고하시면 좋을 것입니다.

## 참고

- [하스켈 `time` 라이브러리 문서](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Modified Julian Day 간단한 소개](https://en.wikipedia.org/wiki/Julian_day#Modified_Julian