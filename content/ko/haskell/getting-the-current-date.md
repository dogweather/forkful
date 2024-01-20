---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:15:09.219806-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
현재 날짜를 가져오는 것은 여러분의 프로그램이 '오늘'이라는 개념을 알 필요가 있을 때 중요합니다. 로그 파일 작성, 데이터 처리, 사용자 경험 개선 등에 사용됩니다.

## How to: (방법)
Haskell에서 현재 날짜를 가져오려면 `Data.Time` 라이브러리를 사용해야 합니다. 간단한 예제는 다음과 같습니다:

```Haskell
import Data.Time

-- 현재 날짜와 시간을 가져옵니다
getCurrentTimeExample :: IO ()
getCurrentTimeExample = do
    current <- getCurrentTime
    print current

-- 오직 날짜 정보만 가져옵니다
getCurrentDateExample :: IO ()
getCurrentDateExample = do
    today <- fmap utctDay getCurrentTime
    print today
```
실행 결과:
```
2023-04-15 15:24:36.123 UTC
2023-04-15
```

## Deep Dive (심층 분석)
Haskell의 `Data.Time` 라이브러리는 시간과 관련된 다양한 기능을 제공합니다. `getCurrentTime` 함수는 UTC로 현재 시각을 반환하는데, 이는 1970년부터 시작된 Unix Time으로부터 계산됩니다. 대안으로, 로컬 타임존을 적용하고 싶다면, `getCurrentTimeZone`과 `utcToLocalTime` 함수를 사용할 수 있습니다.

```Haskell
import Data.Time

getCurrentLocalTimeExample :: IO ()
getCurrentLocalTimeExample = do
  zone <- getCurrentTimeZone
  utc <- getCurrentTime
  let localTime = utcToLocalTime zone utc
  print localTime
```

실행 결과:
```
2023-04-15 18:24:36.123 KST
```

구현상의 특이점으로는, Haskell의 날짜와 시간 함수들은 `IO` 타입을 사용합니다. 이는 실행할 때마다 다를 수 있는 '부수 효과(side effects)'를 가지기 때문입니다. 순수 함수로 시간을 다루지 않는 것이 이 부수 효과 때문에 일어나는 오류를 막을 수 있습니다.

## See Also (더보기)
- `Data.Time` 라이브러리 문서: [Hackage Data.Time](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- 시간 관련 어플리케이션 개발 이론: [Time and its abstractions](https://en.wikipedia.org/wiki/Time_standard)