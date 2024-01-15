---
title:                "두 날짜 비교하기"
html_title:           "Haskell: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

두 개의 날짜를 비교하는 것이 왜 중요한지 궁금하십니까? 일반적인 언어와 달리 Haskell의 날짜 비교 방법은 기술적이고 복잡할 수 있기 때문입니다. 이 글을 통해 Haskell에서의 날짜 비교를 쉽게 이해하고 활용하는 방법을 배우세요.

## 어떻게

Haskell에서 두 날짜를 비교하는 방법은 `compare` 함수를 사용하는 것입니다. 이 함수는 첫 번째 인자를 두 번째 인자와 비교하여 `LT` (less than, 작다), `EQ` (equal, 같다), `GT` (greater than, 크다) 값 중 하나를 반환합니다. 아래 예제를 살펴봅시다.

```Haskell
-- February 18th, 2021
date1 = fromGregorian 2021 2 18
-- March 3rd, 2021
date2 = fromGregorian 2021 3 3

-- 두 날짜를 비교해서 date1이 date2보다 작은지 확인합니다.
compare date1 date2 == LT -- 결과: True
```

위 예제에서 `fromGregorian` 함수는 지정된 년, 월, 일 값을 가지는 `Day` 타입의 날짜를 생성하는 함수입니다. 이를 통해 두 날짜를 생성하고 `compare` 함수를 사용하여 비교하였습니다.

또 다른 방법으로는 `diffDays` 함수를 사용하는 것입니다. 이 함수는 두 날짜 사이의 일 수를 반환합니다. 아래 예제를 살펴봅시다.

```Haskell
-- March 10th, 2021
date1 = fromGregorian 2021 3 10
-- June 15th, 2021
date2 = fromGregorian 2021 6 15

-- 두 날짜 사이의 일 수를 계산합니다.
diffDays date1 date2 -- 결과: 97
```

이 외에도 Haskell에서는 다양한 함수를 사용하여 년, 월, 일, 시간 등의 날짜 정보에 대한 계산을 수행할 수 있습니다. 자세한 내용은 아래 "## 깊숙한 곳으로" 섹션에서 다루도록 하겠습니다.

## 깊숙한 곳으로

날짜를 비교하는 것 외에도 Haskell에서 날짜에 대한 다양한 작업을 수행할 수 있습니다. 다음 라이브러리들을 사용하여 더욱 복잡한 날짜 연산을 수행할 수 있습니다.

- [time 라이브러리](https://hackage.haskell.org/package/time): 날짜, 시간, 시간대에 대한 데이터 타입과 관련 함수를 제공합니다.
- [chronos 라이브러리](https://hackage.haskell.org/package/chronos): 날짜와 시간을 효율적으로 표현하는 라이브러리로, 시간대와 대형 정밀도를 지원합니다.

## 참고하기

- [Haskell 공식 문서](https://www.haskell.org/documentation/)
- [언어 참고사이트](https://www.learnyouahaskell.com/)
- [Haskell에서의 날짜와 시간 다루기](https://en.wikibooks.org/wiki/Haskell/Date_and_time)
- [Haskell에서 날짜와 시간 다루기 - 1](https://blog.felipe.rs/2011/05/04/date-and-time-in-haskell-via-dates-datetime-cputime/)
- [Haskell에서 날짜와 시간 다루기 - 2](https://blog.felipe.rs/2011/12/23/haskell-and-time-handling-parsers-and-comput