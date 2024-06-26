---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:56.338586-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Elm\uC740 \uCF54\uC5B4 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uC5D0 \uB0B4\uC7A5\uB41C regex \uD568\uC218\uB97C \uAC00\uC9C0\uACE0\
  \ \uC788\uC9C0 \uC54A\uC73C\uBA70, \uC774\uB7EC\uD55C \uC5F0\uC0B0\uC744 \uC704\uD574\
  \ \uD0C0\uC0AC \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD574\uC57C \uD569\
  \uB2C8\uB2E4. regex \uC791\uC5C5\uC744 \uC704\uD55C \uC778\uAE30 \uC788\uB294 \uC120\
  \uD0DD \uC911 \uD558\uB098\uB294 `elm/regex`\uC785\uB2C8\uB2E4. `elm install elm/regex`\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC\u2026"
lastmod: '2024-03-13T22:44:55.096622-06:00'
model: gpt-4-0125-preview
summary: "Elm\uC740 \uCF54\uC5B4 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC5D0 \uB0B4\uC7A5\
  \uB41C regex \uD568\uC218\uB97C \uAC00\uC9C0\uACE0 \uC788\uC9C0 \uC54A\uC73C\uBA70\
  , \uC774\uB7EC\uD55C \uC5F0\uC0B0\uC744 \uC704\uD574 \uD0C0\uC0AC \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD574\uC57C \uD569\uB2C8\uB2E4."
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
weight: 11
---

## 사용 방법:
Elm은 코어 라이브러리에 내장된 regex 함수를 가지고 있지 않으며, 이러한 연산을 위해 타사 라이브러리를 사용해야 합니다. regex 작업을 위한 인기 있는 선택 중 하나는 `elm/regex`입니다. `elm install elm/regex`를 사용하여 프로젝트에 추가할 수 있습니다.

다음은 몇 가지 일반적인 작업을 위해 `elm/regex`를 사용하는 방법입니다:

### 1. 패턴 매칭
문자열이 패턴과 일치하는지 확인하기 위해, `Regex.contains`를 사용할 수 있습니다.

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- 사용 예시:
isAlphanumeric "Elm2023"     -- 출력: True
isAlphanumeric "Elm 2023!"   -- 출력: False
```

### 2. 모든 일치 항목 찾기
문자열 내에서 패턴이 일치하는 모든 발생을 찾으려면 `Regex.find`를 사용할 수 있습니다.

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- 사용 예시:
getWords "Elm is fun!"  -- 출력: ["Elm", "is", "fun"]
```

### 3. 텍스트 교체
패턴과 일치하는 문자열의 부분을 교체하기 위해 `Regex.replace`를 사용합니다.

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- 사용 예시:
replaceElmWithHaskell "Learning Elm is fun!"  
-- 출력: "Learning Haskell is fun!"
```

이 예시들에서, `Regex.fromString`은 정규 표현식 패턴을 컴파일하는 데 사용되며, 여기서 `\b`는 단어 경계를 매치하고, `\w`는 모든 단어 문자를 매치합니다. 잘못된 정규 표현식 패턴에 대비해 `Regex.fromString`의 `Maybe` 결과를 항상 처리하며, 이는 보통 `Maybe.withDefault`를 사용합니다.
