---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Haskell: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜를 미래 또는 과거로 계산하는 것은 간단한 작업처럼 보일 수 있지만, 특정 날짜의 연산은 프로그래머들에게 중요합니다. 예를 들어, 날짜를 알아야 할 때, 주로 예약, 기간 계산, 또는 이벤트 예약과 같은 작업을 수행해야 합니다.

## 방법:
```Haskell
-- 미래로부터 날짜 계산
import Data.Time

-- 일 더하기
addDays 1 (fromGregorian 2020 06 01)
--출력: 2020-06-02

-- 월 더하기
addGregorianMonthsClip 1 (fromGregorian 2020 06 01)
--출력: 2020-07-01

-- 미래로부터 날짜 계산
import Data.Time

-- 일 빼기
addDays (-1) (fromGregorian 2020 06 01)
--출력: 2020-05-31

-- 월 빼기
addGregorianMonthsClip (-1) (fromGregorian 2020 06 01)
--출력: 2020-05-01
```

## 깊이 들어가기:
이전에는 날짜를 계산하는 것은 복잡한 작업이었습니다. 그러나 Haskell에서는 Data.Time 라이브러리를 사용하여 간단하게 날짜를 계산할 수 있습니다. 다른 언어에서는 보통 "EPOCH 시간"이라는 날짜 형식을 사용하여 날짜를 계산하지만, Haskell에서는 그렇지 않습니다. Haskell에서는 기온으로 숫자를 사용하여 쉽게 날짜를 계산할 수 있습니다. 또한, addDays와 addGregorianMonthsClip 함수를 통해 날짜를 더하거나 빼는 것도 간단하게 할 수 있습니다.

## 또 다른 예제:
Haskell에서 Date 모듈과 Date.TZ 모듈을 사용하여 시간대를 고려한 날짜 계산을 할 수도 있습니다.

## 참고 자료:
- [Data.Time 라이브러리 공식 홈페이지](https://hackage.haskell.org/package/time)
- [Haskell에서 날짜와 시간 다루기](https://williamyaoh.com/posts/2019-03-25-dates-and-time-haskell.html)
- [TimeZone 라이브러리 공식 홈페이지](https://hackage.haskell.org/package/timezone)