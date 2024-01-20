---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 뭐와 왜?

문자열을 소문자로 변환한다는 것은 모든 대문자를 소문자로 바꾸는 프로세스를 말합니다. 프로그래머들은 코드의 일관성을 유지하고, 검색이나 비교 연산을 간편하게 하기 위해 이를 사용합니다.

## 사용 방법:

`toLower` 함수와 `map` 함수를 이용해서 문자열의 모든 문자를 한 번에 소문자로 바꿀 수 있습니다.

```Haskell
import Data.Char

toLowerString :: String -> String
toLowerString = map toLower
```
보시는 대로, 문자열 `"HELLO"` 를 소문자로 변환하는 예제입니다.
```Haskell
main = print (toLowerString "HELLO")
```

이 코드의 출력 결과는 다음과 같습니다.
```Haskell
"hello"
```

## 깊게 보기:

문자열을 소문자로 변환하는 기능이 필요했던 초기 이유는, 습관적인 텍스트 검색과 비교를 기능성과 속도 측면에서 효율적으로 만드는 것이었습니다. 특히 대소문자가 다르면 다른 문자로 인식하므로, 동일한 단어의 대소문자 형태를 매칭시키기 위해 사용되었습니다.

Haskell에서는 기본적으로 제공하는 `toLower` 함수 외에도, 패키지 "text"를 사용해서 이 기능을 활용할 수 있습니다. 

`toLower` 함수는 Unicode 캐릭터의 소문자 변환을 지원하므로, 대부분의 경우 이 함수를 사용하면 됩니다. 이 함수는 문자를 하나씩 처리하므로, 문자열 전체를 변환하려면 `map` 함수와 함께 사용합니다.

## 연관된 정보:

- [Haskell 'toLower' 공식 문서](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html#v:toLower)
- [Haskell 'map' 함수와의 시너지](https://www.schoolofhaskell.com/school/starting-with-haskell/lists-and-types)
- [텍스트 패키지 사용 방법](https://hackage.haskell.org/package/text)
- [코드의 일관성을 위한 대소문자 변환의 중요성](http://www.drdobbs.com/architecture-and-design/normalization-in-programming-languages/240168319)