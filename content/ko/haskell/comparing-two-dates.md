---
title:                "Haskell: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

두 날짜를 비교하는 일을 진행하게 될 이유는 무엇일까요? 예를 들어, 두 날짜 중 어느 날짜가 더 이전인지 알고 싶거나 특정 시간 간격을 계산하고 싶을 때에는 날짜를 비교하는 것이 필수적입니다. 이러한 요구사항은 프로그래밍에서도 주로 발생하므로 두 날짜를 비교하는 방법을 알아봅시다.

## 어떻게

우리는 Haskell 언어를 사용하여 두 날짜를 비교하는 방법을 알아볼 것입니다. 먼저, `import Data.Time.Calendar`를 통해 날짜와 관련된 함수를 불러옵니다.

### 날짜 생성하기

`fromGregorian` 함수를 사용하여 `year month day` 순서로 날짜를 생성합니다. 예를 들어, `fromGregorian 2021 11 18`은 2021년 11월 18일을 나타냅니다.

```Haskell
fromGregorian :: Integer -> Int -> Int -> Day
-- fromGregorian year month day = Day
```

### 날짜 비교하기

`compare` 함수를 사용하여 두 날짜를 비교할 수 있습니다. 이 함수는 `Ordering` 타입을 반환하며, `LT`, `EQ`, `GT` 중 하나의 값을 가집니다.

```Haskell
compare :: Day -> Day -> Ordering
-- compare d1 d2 = Ordering
```

아래는 이를 응용한 예시입니다.

```Haskell
compareDates :: Day -> Day -> String
compareDates d1 d2 =
  case compare d1 d2 of
    LT -> "First date is earlier than second date."
    EQ -> "Both dates are the same."
    GT -> "First date is later than second date."

d1 = fromGregorian 2021 11 18
d2 = fromGregorian 2021 11 19

compareDates d1 d2 -- "First date is earlier than second date."
```

## 깊게 파고들기

우리는 위에서 `compare` 함수를 사용하여 두 날짜를 비교할 수 있다는 것을 알게 되었습니다. 이를 응용하면 두 날짜 간의 차이를 계산하는 것도 가능합니다. `diffDays` 함수를 사용하여 두 날짜 사이의 일 수 차이를 계산할 수 있습니다.

```Haskell
diffDays :: Day -> Day -> Integer
-- diffDays d1 d2 = Integer
```

아래는 이를 응용한 예시입니다.

```Haskell
daysBetween :: Day -> Day -> String
daysBetween d1 d2 =
  let diff = abs (diffDays d1 d2)
  in "Number of days between the two dates: " ++ show diff

d1 = fromGregorian 2021 11 18
d2 = fromGregorian 2021 11 20

daysBetween d1 d2 -- "Number of days between the two dates: 2"
```

## 추가 자료

위에서 다룬 내용 외에도 Haskell에서 날짜를 다루는 다양한 함수가 있습니다. 앞서 언급한 `Data.Time` 모듈 외에도 `Data.Time.Clock`, `Data.Time.Calendar.OrdinalDate` 등의 모듈에서도 유용한 함수들을 찾을 수 있습니다.

## 참고 자료

- [Haskell 공식 문서](https://www.haskell.org/documentation/)
- [Haskell Date and Time Library](https://www.haskell.org/haskellwiki/Date_and_time_library)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)