---
title:                "Haskell: 두 날짜 비교하기"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜

날짜 비교는 프로그래밍에서 매우 일반적인 작업입니다. 날짜를 비교하면 특정 날짜 이전에 무슨 일이 발생했는지 또는 두 날짜의 차이를 계산할 수 있습니다. 날짜 비교는 일상 생활에서도 자주 사용되는 기능이기 때문에 학습하는 것이 중요합니다.

# 방법

날짜를 비교하는 방법은 간단합니다. 먼저 날짜 데이터를 정확하게 파싱하는 것이 중요합니다. Haskell에서는 `Date`나 `DateTime`와 같은 라이브러리를 사용하여 날짜를 쉽게 다룰 수 있습니다. 다음으로는 적절한 연산자를 사용하여 두 날짜를 비교하고 비교 결과를 출력하면 됩니다.

```Haskell
import Data.Time.Clock
import Data.Time.Calendar

-- 날짜 비교 예제
date1 = fromGregorian 2021 03 10
date2 = fromGregorian 2021 06 15

-- `dayNumber` 함수를 사용하여 두 날짜의 차이를 일 수로 계산
dayNumber = diffDays date2 date1

-- 일 수 출력
print dayNumber
```

위의 코드를 실행하면 다음과 같은 결과가 나옵니다.

```
97
```

이는 `dayNumber` 변수에 저장된 두 날짜의 차이가 97일이라는 의미입니다. 날짜를 비교할 때는 원하는 날짜 형식으로 데이터를 파싱하고, 정확한 연산자와 함수를 사용하는 것이 중요합니다.

# 깊이 파헤치기

날짜 비교를 하면서 발생할 수 있는 다양한 상황들을 더 자세히 살펴보겠습니다. Haskell에서는 `diffDays` 함수 외에도 `diffLocalTime` 함수를 사용하여 두 날짜 사이의 시간 차이를 계산할 수 있습니다. 또한 `fromGregorianValid` 함수를 사용하여 유효한 날짜인지 확인할 수도 있습니다. 더 많은 함수들을 사용하여 날짜 비교를 보다 정확하게 할 수 있습니다.

# 참고자료

- [Haskell 공식 문서 - 날짜 비교](https://hackage.haskell.org/package/time/docs/Data-Time-Clock.html)
- [Learn You a Haskell - 날짜와 시간](http://learnyouahaskell.com/types-and-typeclasses#typeclasses-101)
- [Real World Haskell - 날짜와 시간 다루기](http://book.realworldhaskell.org/read/using-typeclasses.html#date-and-time)