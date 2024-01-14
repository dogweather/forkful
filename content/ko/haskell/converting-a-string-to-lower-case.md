---
title:                "Haskell: 문자열을 소문자로 변환하기"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것이 왜 중요한지 궁금하신가요? 이 블로그 포스트에서는 이 질문에 대한 답을 찾아보겠습니다.

## 어떻게 하나요?

우리는 Haskell으로 문자열을 소문자로 변환하는 방법을 알아볼 것입니다. 아래의 예시 코드와 함께 따라와주세요.

```Haskell
import Data.Char

-- 문자열을 소문자로 변환하는 함수
lowercaseString :: String -> String
lowercaseString str = map toLower str

-- 예시 입력과 출력
"한글" -> "한글"
"Hello World" -> "hello world"
```

위의 예시 코드에서 우리는 `Data.Char` 모듈에서 제공하는 `toLower` 함수를 사용하여 문자열을 소문자로 변환하는 함수를 정의했습니다. `map` 함수를 사용하여 문자열의 각 문자를 `toLower` 함수에 적용시켜주면 됩니다.

## 더 자세히 알아보기

Haskell에서 문자열은 기본적으로 유니코드로 인코딩되어 있습니다. 따라서 `toLower` 함수는 유니코드 문자를 올바르게 소문자로 변환해줍니다. 또한, `toLower` 함수는 영어 뿐만 아니라 다른 언어의 문자도 올바르게 변환해줄 수 있도록 설계되어 있습니다.

## 더 알아보기

만약 리스트나 튜플 등의 다른 자료형에서도 소문자로 변환하고 싶다면 어떻게 해야할까요? `Data.Char` 모듈에는 `toLower` 함수와 함께 `toLower` 함수와 같이 작동하는 다른 함수들도 많이 있으니, 참고하시면 좋을 것 같습니다.

## 참고자료

- [Haskell Data.Char 모듈 문서](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Haskell 표준 라이브러리 문서](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/)
- [Haskell 문자열 관련 함수 목록](https://www.tutorialspoint.com/haskell/haskell_strings.htm)

## 참고하기

- [Haskell 문자열 다루기](https://codedragon.tistory.com/5026)
- [문자열을 소문자로 변환하는 또 다른 방법 - Hoogle 검색하기](https://www.haskell.org/hoogle/?hoogle=toLower)