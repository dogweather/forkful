---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇이고 왜합니까? (What & Why?)

패턴과 일치하는 문자 삭제는, 이름에서 알 수 있듯이, 특정 패턴과 일치하는 문자를 문자열에서 제거하는 작업입니다. 이 작업은 보통 불필요한 문자나 문자열을 제거하여 데이터를 정리하고 가공하는 데 사용됩니다.

## 어떻게 합니까? (How to?)

여기 Haskell에서 패턴과 일치하는 문자를 삭제하는 간단한 예시를 보여드리겠습니다.

```Haskell
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

main = putStrLn (trim "   Hello, World!   ")

-- Output: "Hello, World!"
```

위의 코드는 문자열의 앞과 뒤에서 공백 문자를 삭제합니다.

## 깊이 있는 정보 (Deep Dive)

패턴과 일치하는 문자를 삭제하는 이 개념은 결코 새로운 것이 아닙니다. 이는 컴퓨터 프로그래밍 초기부터 프로그래머들이 데이터를 보다 쉽게 처리하고 분석할 수 있도록 도와주었습니다.

Haskell 외에도, 대부분의 프로그래밍 언어는 특정 패턴에 일치하는 문자를 삭제하는 방법을 제공합니다. Python에서는 `re.sub()`를, JavaScript에서는 `replace()`를 사용하여 유사한 작업을 수행할 수 있습니다.

이런 기능은 문자열 처리 작업의 일부로 구현된 것이며 현재 Haskell의 기본 라이브러리인 `Data.List`와 `Data.Char`에서도 지원되고 있습니다.

## 참고 자료 (See Also)

패턴과 일치하는 문자 삭제에 대해 더 배우고 싶다면, 다음의 링크들을 참조해주시기 바랍니다.

1. Haskell의 공식 문서: http://haskell.org/
2. "Learn You a Haskell for Great Good": http://learnyouahaskell.com/
3. Stack Overflow의 "How to delete a character from a string using Haskell?": https://stackoverflow.com/questions/19184856/how-to-delete-a-character-from-a-string-using-haskell