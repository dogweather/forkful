---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:45.018412-07:00
description: "\uBC29\uBC95: Haskell\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uC778 `base`\uB294 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uB2E4\uB8E8\uB294 \uAE30\
  \uB2A5\uC744 \uC81C\uACF5\uD558\uB294 `Data.Time` \uBAA8\uB4C8\uC744 \uD3EC\uD568\
  \uD558\uACE0 \uC788\uC2B5\uB2C8\uB2E4. \uC5EC\uAE30\uC5D0 \uD604\uC7AC \uB0A0\uC9DC\
  \uB97C \uC5BB\uB294 \uBC29\uBC95\uC774 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.310973-06:00'
model: gpt-4-0125-preview
summary: "Haskell\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC778 `base`\uB294\
  \ \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uB2E4\uB8E8\uB294 \uAE30\uB2A5\uC744 \uC81C\
  \uACF5\uD558\uB294 `Data.Time` \uBAA8\uB4C8\uC744 \uD3EC\uD568\uD558\uACE0 \uC788\
  \uC2B5\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

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
