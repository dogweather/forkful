---
title:                "문자열을 소문자로 변환하기"
html_title:           "Haskell: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
스트링을 소문자로 변환한다는 것은 무엇인지 그리고 왜 프로그래머가 이를 하는지에 대해 간단히 설명합니다.

스트링을 소문자로 변환하는 것은 문자열의 대소문자를 구분하지 않는 비교나 처리를 위해 필요한 작업입니다. 이를 통해 입력된 값의 형식을 통일하여 정보를 처리하고 결과를 추론하기 쉽게 만들 수 있습니다. 게다가 일부 프로그래밍 언어에서는 대소문자를 모두 허용하지 않기 때문에, 소문자로 문자열을 바꾸는 것은 반드시 필수적인 작업입니다.

## 어떻게:
Haskell에서 이 작업을 수행하는 방법을 코드 예시와 함께 살펴봅니다. 

```
-- 문자열의 모든 문자를 소문자로 변환하는 함수
toLower :: String -> String
toLower str = [toLowerChar c | c <- str]

-- 문자 하나를 소문자로 변환하는 함수
toLowerChar :: Char -> Char
toLowerChar c
  | isUpper c = chr (ord c + 32) -- ASCII 코드를 이용해 대문자를 소문자로 변환
  | otherwise = c
```

위 코드를 실행하면, "HASKELL"이라는 문자열이 "haskell"로 변환된 것을 확인할 수 있습니다.

## 깊이 파고들기:
스트링을 소문자로 변환하는 과정은 이전에 ASCII 코드를 이용했던 방식을 바탕으로 만들어졌습니다. 그러나 이는 현대 프로그래밍 언어에서도 여전히 널리 사용되고 있습니다. 다른 언어들에서는 대소문자를 구분하지 않는 문자열 타입을 제공하기도 합니다. 더 나아가, 고급 프로그래밍 언어에서는 함수형 프로그래밍과 같은 패러다임을 통해 간편한 문자열 처리를 위해 파생된 함수도 제공하고 있습니다. 이를 이용하면 더욱더 손쉽게 문자열을 소문자로 변환할 수 있습니다.

## 참고 자료:
스트링을 소문자로 변환하는 더 많은 정보를 알고 싶다면 아래 링크를 참고하세요.

- [Haskell에서 제공하는 문자열 처리 함수](https://hoogle.haskell.org/)
- [파이썬과 같은 다른 프로그래밍 언어에서 문자열 처리하기](https://www.w3schools.com/python/ref_string_lower.asp)
- [자바스크립트에서 문자열을 소문자로 변환하는 방법](https://www.w3schools.com/js/js_string_lower.asp)