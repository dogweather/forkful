---
title:                "문자열 소문자로 변환하기"
html_title:           "Haskell: 문자열 소문자로 변환하기"
simple_title:         "문자열 소문자로 변환하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것은 문자열 처리를 하거나 더 많은 언어 기능을 활용하기 위해 필요할 수 있습니다.

## 어떻게 하나요?

```Haskell
import Data.Char (toLower)

-- toLower 함수를 사용하여 문자열을 소문자로 변환
lowercaseString :: String -> String
lowercaseString str = map toLower str

-- 예시 출력
> lowercaseString "HELLO WORLD"
"hello world"
```

## 깊이 파고들기

문자열을 소문자로 변환하는 데는 여러 가지 방법이 있지만 "Data.Char" 모듈의 "toLower" 함수를 사용하는 것이 가장 간단하고 효율적입니다. 이 함수는 주어진 문자를 대응하는 소문자로 변환해주는 역할을 합니다. 이를 이용하면 문자열을 한 번에 소문자로 변환할 수 있습니다.

## See Also

- [Haskell 공식 홈페이지](https://www.haskell.org/)
- [Haskell 한국어 커뮤니티](https://www.reddit.com/r/HaskellKR/)