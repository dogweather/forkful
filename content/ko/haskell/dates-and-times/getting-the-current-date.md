---
title:                "현재 날짜 가져오기"
aliases:
- /ko/haskell/getting-the-current-date.md
date:                  2024-02-03T19:09:45.018412-07:00
model:                 gpt-4-0125-preview
simple_title:         "현재 날짜 가져오기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며, 왜?
Haskell에서 현재 날짜를 검색하는 것은 시스템의 현재 시간을 얻고 이를 읽을 수 있는 날짜 형식으로 변환하는 것을 포함합니다. 프로그래머들은 로깅, 작업 스케줄링 또는 애플리케이션에서 이벤트 타임스탬핑과 같은 날짜를 기반으로 한 작업을 수행하기 위해 이 작업을 합니다.

## 방법:
Haskell의 표준 라이브러리인 `base`는 날짜와 시간을 다루는 기능을 제공하는 `Data.Time` 모듈을 포함하고 있습니다. 여기에 현재 날짜를 얻는 방법이 있습니다:

```haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

샘플 출력:
```
2023-04-12
```

날짜를 포맷팅하거나 다른 시간대와 작업하는 등의 더 많은 유연성을 위해서는 `time` 라이브러리가 매우 유용합니다. 현재 날짜를 포맷하는 방법은 다음과 같습니다:

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" zoneNow
```

이 코드는 현재 날짜를 지역 시간대에 맞춰 `YYYY-MM-DD` 형식으로 출력합니다.

추가적으로, 타사 라이브러리 지원이 필요한 경우, Haskell 커뮤니티에서 광범위한 날짜 및 시간 조작 기능으로 자주 사용되는 `time`을 사용하는 것이 권장됩니다. 위의 예제들은 이 라이브러리를 사용합니다.

문자열에서 파싱하는 것이나 날짜와 시간과의 산술 연산 등 보다 포괄적인 날짜 조작이 필요한 경우, `Data.Time` 내의 추가 함수를 탐색하는 것이 유익할 것입니다.
