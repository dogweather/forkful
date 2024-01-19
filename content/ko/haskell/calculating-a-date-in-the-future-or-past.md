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

## 뭐하고 왜 하나요?

'미래나 과거의 날짜 계산하기'는 특정 날짜에서 특정 일수를 더하거나 빼서 새로운 날짜를 찾는 것입니다. 프로그래머들은 이를 사용하여 날짜 관련 문제를 해결하거나, 특정 이벤트의 발생 시점을 예측하는 데 사용합니다.

## 어떻게 만드나요:

```Haskell
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)
import Data.Time.Calendar (Day, addDays)

futureDate :: UTCTime -> NominalDiffTime -> UTCTime
futureDate time duration = addUTCTime duration time

pastDate :: Day -> Integer -> Day
pastDate date days = addDays (-days) date
```

다음과 같이 실행하면 출력을 얻을 수 있습니다.

```Haskell
futureDate someUTCTime (daysToNominalDiffTime 7)
pastDate someDate 30
```

## 깊게 들어가기

'미래나 과거의 날짜 계산하기'는 컴퓨터가 탄생하기 이전부터 사용되어 왔습니다. 원래는 필기로 수행되었지만, 이제는 대부분의 언어가 내장 기능 혹은 라이브러리를 통해 제공합니다. Haskell에서는 `Data.Time.Clock`와 `Data.Time.Calendar` 패키지를 사용하여 간단하게 수행할 수 있습니다.

대안으로, 우리는 날짜 간의 경과일을 계산하는데 사용될 수 있는 `diffDays` 함수를 사용할 수 있습니다.

```Haskell
import Data.Time.Calendar (Day, diffDays)

daysBetween :: Day -> Day -> Integer
daysBetween day1 day2 = diffDays day1 day2
```

이 구현의 세부 사항은 `Julian Day Numbers (JDN)` 시스템에 얽힌 것입니다, 이 시스템은 날짜 수치 연산에 일반적으로 사용되며 Haskell의 `Day`는 내부적으로 이를 사용합니다.

## 참고 할 수 있는 링크

- [Haskell Date and Time Guide](https://two-wrongs.com/haskell-time-library-tutorial)
- [Learn You a Haskell: Library documentation](http://hackage.haskell.org/package/time)
- [Julian Day Numbers explained](https://en.wikipedia.org/wiki/Julian_day)