---
title:                "현재 날짜 가져오기"
html_title:           "Haskell: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 현재 날짜란 무엇인가?

Haskell에는 현재 날짜를 얻는 기능이 내장되어 있습니다. 여러분이 알고 있는 대부분의 프로그래밍 언어에서와 마찬가지로, "현재 날짜"란 오늘의 날짜와 시간을 의미합니다. 프로그래머들은 주로 현재 날짜를 얻어서 다양한 시간 관련 작업을 수행하기 위해 사용합니다.

## 어떻게 하나요?

아래의 코드는 Haskell에서 현재 날짜를 얻는 간단한 예제입니다. 이 코드를 실행하면 현재 시간과 날짜가 출력됩니다.

```haskell
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

main = do
    time <- getCurrentTime
    timezone <- getCurrentTimeZone
    let localTime = utcToLocalTime timezone time
    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime
    putStrLn formattedTime
```

출력 예시:

```
2021-07-17 22:30:00
```

## 더 깊게 알아보기

현재 날짜를 얻는 기능은 Haskell에서 이미 구현되어 있기 때문에 외부 라이브러리를 사용할 필요가 없습니다. 그러나 이 기능의 구현방식은 시스템에 따라 다를 수 있으므로, 외부 라이브러리를 사용하는 경우 시스템 간의 일관성을 유지하기 위해 외부 라이브러리를 사용하는 것이 좋습니다.

## 관련 자료

- [Haskell 공식 문서 - Data.Time.Clock 모듈](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html)
- [Haskell 공식 문서 - Data.Time.Format 모듈](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Haskell 공식 문서 - Data.Time.LocalTime 모듈](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html)