---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이고, 왜 사용하나요?

문자열 연결은 여러 개의 문자열을 하나로 결합하는 것을 의미합니다. 이는 데이터를 적절하게 형식화하거나 결과를 출력하기 위해 프로그래머들이 많이 사용합니다.

## 어떻게 하나요:

Haskell에서 문자열 연결을 하는 방법에는 주로 두 가지가 있습니다. 

```Haskell
-- (++) 연산자를 이용한 방법
main = putStrLn ("Hello " ++ "World!")
-- 출력: Hello World!
```
또는 `concat` 함수를 사용하여 더 복잡한 문자열 연결을 할 수 있습니다.

```Haskell
main = putStrLn (concat ["Hello ", "World!"])
-- 출력: Hello World!
```

## 심층 탐구:

Haskell의 문자열 연결 방식은 다른 언어들의 방식과 다소 차이가 있습니다. 이는 이전 함수형 언어와 그 배경에 따른 것입니다. 대안으로는 `intercalate` 함수를 사용하여 리스트에 있는 문자열 사이에 다른 문자열을 삽입하여 연결할 수 있습니다.

```Haskell
import Data.List
main = putStrLn (intercalate " " ["Hello", "World!"])
-- 출력: Hello World!
```
    
문자열 연결 구현의 세부 사항에 대해서는 리스트 연산이 어떻게 작동하는지를 이해하면 됩니다. Haskell에서 문자열은 문자의 리스트입니다. 따라서 문자열 연결은 리스트 연결이며, 이는 복잡성이 `O(n)` 입니다.

## 참고 자료:

다음은 문자열 연결에 대한 더 많은 정보와 관련 학습 자료에 대한 링크들입니다:
1. haskell.org에 있는 문자열 연결에 관한 설명: https://wiki.haskell.org/Concatenate_two_lists
2. Stack Overflow에서의 관련 질문: https://stackoverflow.com/questions/9156393/concatenation-in-haskell
3. "Learn You a Haskell for Great Good"의 문자열 섹션: http://learnyouahaskell.com/starting-out#an-intro-to-lists