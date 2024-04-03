---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:29.584560-07:00
description: "\uC5B4\uB5BB\uAC8C: \uAE30\uBCF8\uC801\uC73C\uB85C, Haskell\uC740 \uB0A0\
  \uC9DC\uB97C \uD30C\uC2F1\uD558\uAE30 \uC704\uD55C \uAE30\uBCF8 \uB3C4\uAD6C\uB97C\
  \ \uC81C\uACF5\uD558\uC9C0\uB9CC, \uD575\uC2EC \uAE30\uB2A5\uC744 \uC704\uD55C `time`\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC640 \uB354 \uC720\uC5F0\uD55C \uD30C\uC2F1\uC744\
  \ \uC704\uD55C `date-parse` \uB610\uB294 `time-parse` \uAC19\uC740 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uB97C \uD65C\uC6A9\uD558\uBA74 \uC791\uC5C5\uC744 \uC0C1\uB2F9\uD788\
  \ \uB2E8\uC21C\uD654\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uBA3C\uC800\u2026"
lastmod: '2024-03-13T22:44:55.309310-06:00'
model: gpt-4-0125-preview
summary: "\uAE30\uBCF8\uC801\uC73C\uB85C, Haskell\uC740 \uB0A0\uC9DC\uB97C \uD30C\uC2F1\
  \uD558\uAE30 \uC704\uD55C \uAE30\uBCF8 \uB3C4\uAD6C\uB97C \uC81C\uACF5\uD558\uC9C0\
  \uB9CC, \uD575\uC2EC \uAE30\uB2A5\uC744 \uC704\uD55C `time` \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uC640 \uB354 \uC720\uC5F0\uD55C \uD30C\uC2F1\uC744 \uC704\uD55C `date-parse`\
  \ \uB610\uB294 `time-parse` \uAC19\uC740 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD65C\
  \uC6A9\uD558\uBA74 \uC791\uC5C5\uC744 \uC0C1\uB2F9\uD788 \uB2E8\uC21C\uD654\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 어떻게:
기본적으로, Haskell은 날짜를 파싱하기 위한 기본 도구를 제공하지만, 핵심 기능을 위한 `time` 라이브러리와 더 유연한 파싱을 위한 `date-parse` 또는 `time-parse` 같은 라이브러리를 활용하면 작업을 상당히 단순화할 수 있습니다.

먼저 `time` 라이브러리를 사용할 수 있도록 합니다; 이것은 종종 GHC와 함께 포함되어 있지만, 의존성으로 명시해야 할 필요가 있다면 프로젝트의 cabal 파일에 `time`을 추가하거나 `cabal install time`을 사용해 수동으로 설치하세요.

```haskell
import Data.Time.Format
import Data.Time.Clock
import System.Locale (defaultTimeLocale)

-- time 라이브러리를 사용해 표준 형식의 날짜를 파싱하기
parseBasicDate :: String -> Maybe UTCTime
parseBasicDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" 
```

예제 사용 및 출력:

```haskell
main :: IO ()
main = print $ parseBasicDate "2023-04-01"

-- 출력: Just 2023-03-31 22:00:00 UTC
```

여러 형식이나 로케일을 처리해야 하는 보다 복잡한 상황에서는, `date-parse`와 같은 서드파티 라이브러리가 더 편리할 수 있습니다:

`date-parse`를 의존성에 추가하고 설치했다고 가정하면, 다음과 같이 사용할 수 있습니다:

```haskell
import Data.Time.Calendar
import Text.Date.Parse (parseDate)

-- date-parse 라이브러리를 사용해 날짜 문자열을 파싱하면 다양한 형식을 지원합니다
parseFlexibleDate :: String -> Maybe Day
parseFlexibleDate = parseDate
```

`date-parse`를 사용한 예제:

```haskell
main :: IO ()
main = print $ parseFlexibleDate "April 1, 2023"

-- 출력: Just 2023-04-01
```

각 예제는 문자열을 Haskell에서 사용 가능한 날짜 객체로 변환하는 기본적인 접근 방식을 보여 줍니다. `time` 라이브러리의 내장 함수를 사용하거나 `date-parse` 같은 서드파티 솔루션을 선택하는 것은 처리해야 할 입력 형식의 범위와 같은 애플리케이션의 특정 요구 사항에 따라 달라집니다.
