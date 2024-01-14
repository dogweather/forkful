---
title:                "Haskell: 날짜를 문자열로 변환하기"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜: 날짜를 문자열로 변환하는 데 참여하는 이유

날짜를 문자열로 변환하는 것은 프로그래밍에서 중요한 작업입니다. 이 작업을 할 때 많은 이유가 있을 수 있지만, 일반적으로 날짜를 사용자가 이해하고 처리하기 쉬운 형식으로 표시하기 위해서입니다.

## 하는 법: Haskell에서 날짜를 문자열로 변환하는 방법

```Haskell
-- Data.Time 모듈 가져오기
import Data.Time

-- 함수 정의
convertToString :: Day -> String
convertToString = showGregorian
```

위의 코드는 날짜를 기본적으로 사용되는 그레고리오력(Gregorian calendar)으로 표시하는 기능을 가진 함수를 보여줍니다. 따라서 다음과 같이 사용할 수 있습니다.

```Haskell
convertToString (fromGregorian 2021 10 31) -- "2021-10-31"
```

보다 복잡한 날짜 형식을 사용하고 싶다면, Data.Time.Format 모듈을 사용하여 원하는 형식의 문자열로 변환할 수 있습니다.

## 더 깊게 파헤치기: 날짜를 문자열로 변환하는 더 깊은 정보

날짜를 문자열로 변환할 때, 부동 소수점 방식이 문제가 될 수 있습니다. 예를 들어, 2021년 10월 31일을 `2021.10.31`과 같은 형태로 표시하려고 할 때, 데이터 유실이 발생할 수 있습니다. 따라서, Haskell에서는 `showGregorian` 함수를 사용하는 것보다 `formatTime` 함수를 사용하는 것이 더 안전합니다.

```Haskell
-- Data.Time.Format 모듈 가져오기
import Data.Time.Format

-- 함수 정의
convertToCustomString :: Day -> String
convertToCustomString = formatTime defaultTimeLocale "%Y.%m.%d."
```

위의 코드는 날짜를 `YYYY.MM.DD.` 형식의 문자열로 변환하는 함수를 보여줍니다. 따라서 다음과 같이 사용할 수 있습니다.

```Haskell
convertToCustomString (fromGregorian 2021 10 31) -- "2021.10.31."
```

이처럼 더 복잡한 형식으로 날짜를 표시하고 싶을 때는 `formatTime` 함수를 사용하면 됩니다.

## 참고하기: 날짜를 문자열로 변환하는 관련 링크들

- [Haskell 공식 문서 - Data.Time 모듈](https://haskell.org/ghc/docs/latest/html/libraries/time-1.9.3/Data-Time.html)
- [Haskell 공식 문서 - Data.Time.Format 모듈](https://haskell.org/ghc/docs/latest/html/libraries/time-1.9.3/Data-Time-Format.html)
- [HaskellWiki - 날짜와 시간 처리](https://wiki.haskell.org/Date_and_time)