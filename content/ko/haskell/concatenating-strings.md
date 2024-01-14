---
title:                "Haskell: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 더하는 것을 배우는 것이 왜 중요한지 궁금했나요? 문자열을 더하는 것은 프로그램에서 자주 발생하는 작업 중 하나입니다. 예를 들면 사용자로부터 입력을 받아서 메시지를 출력할 때, 여러 개의 문자열을 이어서 보여주는 경우가 많아요.

## 방법

우선 Haskell 설치가 되어있어야 해요. 그리고 이어서 `ghci` 콘솔 창을 열어서 아래 예제 코드를 입력해보세요.

```Haskell
"Hello" ++ " " ++ "world" -- "Hello world"

"1" ++ "1" -- "11"

"a" ++ "b" ++ "c" -- "abc"
```

보시는 것처럼 `++`를 사용하면 문자열을 이어서 출력할 수 있어요. 이때 양쪽에 있는 문자열을 합치는 것이므로, 기존의 문자열이 수정되지는 않아요.

## 딥 다이브

`++` 연산자는 내부에서 어떻게 작동할까요? Haskell에서는 문자열을 단순히 문자들의 리스트로 처리합니다. 따라서 `++`는 두 리스트를 이어서 새로운 리스트를 만드는 함수입니다. 이 함수는 두 경우로 나뉩니다. 하나는 빈 리스트와 문자열을 이어주는 경우, 다른 하나는 마지막 글자를 제외한 문자열을 받아서 마지막 글자를 추가해주는 경우입니다.

다른 언어에서는 문자열을 이어주는 함수가 따로 존재할 수도 있지만, Haskell에서는 문자열을 리스트로 취급하므로 일관성있게 `++` 함수를 사용할 수 있어요.

## 관련 자료

[Learn You a Haskell](http://learnyouahaskell.com/), [Haskell String concatenation](https://www.geeksforgeeks.org/haskell-string-concatenation/), [Real World Haskell](http://book.realworldhaskell.org/)