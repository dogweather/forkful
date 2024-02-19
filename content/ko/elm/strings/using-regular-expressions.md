---
aliases:
- /ko/elm/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:56.338586-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\
  \uC2DD(regex)\uC740 \uBB38\uC790\uC5F4 \uB0B4\uC5D0\uC11C \uBB38\uC790 \uC870\uD569\
  \uC744 \uB9E4\uCE6D\uD558\uAE30 \uC704\uD574 \uC0AC\uC6A9\uB418\uB294 \uD328\uD134\
  \uC785\uB2C8\uB2E4. Elm\uC5D0\uC11C\uB3C4 \uB2E4\uB978 \uC5B8\uC5B4\uC640 \uB9C8\
  \uCC2C\uAC00\uC9C0\uB85C, \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC785\uB825\
  \ \uAC80\uC99D, \uAC80\uC0C9, \uADF8\uB9AC\uACE0 \uBB38\uC790\uC5F4 \uB0B4\uC758\
  \ \uD14D\uC2A4\uD2B8 \uAD50\uCCB4 \uAC19\uC740 \uC791\uC5C5\uC744 \uC218\uD589\uD558\
  \uAE30 \uC704\uD574 \uADF8 \uC720\uC5F0\uC131\uACFC \uD6A8\uC728\uC131 \uB54C\uBB38\
  \uC5D0 regex\uB97C\u2026"
lastmod: 2024-02-18 23:09:06.070825
model: gpt-4-0125-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\uC2DD\
  (regex)\uC740 \uBB38\uC790\uC5F4 \uB0B4\uC5D0\uC11C \uBB38\uC790 \uC870\uD569\uC744\
  \ \uB9E4\uCE6D\uD558\uAE30 \uC704\uD574 \uC0AC\uC6A9\uB418\uB294 \uD328\uD134\uC785\
  \uB2C8\uB2E4. Elm\uC5D0\uC11C\uB3C4 \uB2E4\uB978 \uC5B8\uC5B4\uC640 \uB9C8\uCC2C\
  \uAC00\uC9C0\uB85C, \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC785\uB825 \uAC80\
  \uC99D, \uAC80\uC0C9, \uADF8\uB9AC\uACE0 \uBB38\uC790\uC5F4 \uB0B4\uC758 \uD14D\uC2A4\
  \uD2B8 \uAD50\uCCB4 \uAC19\uC740 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uAE30 \uC704\
  \uD574 \uADF8 \uC720\uC5F0\uC131\uACFC \uD6A8\uC728\uC131 \uB54C\uBB38\uC5D0 regex\uB97C\
  \u2026"
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
프로그래밍에서 정규 표현식(regex)은 문자열 내에서 문자 조합을 매칭하기 위해 사용되는 패턴입니다. Elm에서도 다른 언어와 마찬가지로, 프로그래머들은 입력 검증, 검색, 그리고 문자열 내의 텍스트 교체 같은 작업을 수행하기 위해 그 유연성과 효율성 때문에 regex를 사용합니다.

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
