---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:45.018412-07:00
description: "Haskell\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC80\uC0C9\uD558\
  \uB294 \uAC83\uC740 \uC2DC\uC2A4\uD15C\uC758 \uD604\uC7AC \uC2DC\uAC04\uC744 \uC5BB\
  \uACE0 \uC774\uB97C \uC77D\uC744 \uC218 \uC788\uB294 \uB0A0\uC9DC \uD615\uC2DD\uC73C\
  \uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB85C\uAE45, \uC791\uC5C5 \uC2A4\uCF00\uC904\
  \uB9C1 \uB610\uB294 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uC774\uBCA4\
  \uD2B8 \uD0C0\uC784\uC2A4\uD0EC\uD551\uACFC \uAC19\uC740 \uB0A0\uC9DC\uB97C \uAE30\
  \uBC18\uC73C\uB85C \uD55C \uC791\uC5C5\uC744 \uC218\uD589\uD558\uAE30 \uC704\uD574\
  \ \uC774 \uC791\uC5C5\uC744 \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.310973-06:00'
model: gpt-4-0125-preview
summary: "Haskell\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC80\uC0C9\uD558\uB294\
  \ \uAC83\uC740 \uC2DC\uC2A4\uD15C\uC758 \uD604\uC7AC \uC2DC\uAC04\uC744 \uC5BB\uACE0\
  \ \uC774\uB97C \uC77D\uC744 \uC218 \uC788\uB294 \uB0A0\uC9DC \uD615\uC2DD\uC73C\uB85C\
  \ \uBCC0\uD658\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uB85C\uAE45, \uC791\uC5C5 \uC2A4\uCF00\uC904\uB9C1\
  \ \uB610\uB294 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uC774\uBCA4\uD2B8\
  \ \uD0C0\uC784\uC2A4\uD0EC\uD551\uACFC \uAC19\uC740 \uB0A0\uC9DC\uB97C \uAE30\uBC18\
  \uC73C\uB85C \uD55C \uC791\uC5C5\uC744 \uC218\uD589\uD558\uAE30 \uC704\uD574 \uC774\
  \ \uC791\uC5C5\uC744 \uD569\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
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
