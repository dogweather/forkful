---
title:    "Haskell: 문자열 대문자화하기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 첫 글자를 대문자로 바꾸는 작업은 다른 프로그래밍 언어에서도 자주 사용되는 작업입니다. 하지만 Haskell에서는 이 작업을 더 쉽고 간결하게 할 수 있습니다. 이 글에서는 이 작업에 대해 자세히 알아보고, 이를 통해 Haskell의 강력함을 알아보겠습니다.

## 방법

우선 `Data.Char` 모듈을 불러와야 합니다. 그 후, `toUpper` 함수를 사용하여 문자열의 첫 글자를 대문자로 바꿀 수 있습니다. 아래는 이 과정을 보여주는 예시 코드입니다.

```Haskell
import Data.Char

capitalize :: String -> String
capitalize str = toUpper (head str) : tail str

main :: IO ()
main = do
  let str = "hello, world"
  let newStr = capitalize str
  putStrLn newStr
```

위 코드를 실행하면 "Hello, world"라는 문자열이 출력됩니다. `import` 키워드를 사용하여 `Data.Char` 모듈을 불러왔고, `capitalize`라는 함수를 정의했습니다. 이 함수는 첫 번째 인자로 받은 문자열의 첫 글자를 대문자로 바꾸어 줍니다. `toUpper` 함수를 사용하면 문자를 대문자로 바꿀 수 있으며, `head` 함수를 이용해 문자열의 첫 글자를 가져옵니다. 마지막으로 `tail` 함수를 이용해 첫 글자를 제외한 나머지 문자열을 구합니다. 이렇게 구해진 문자열들을 `:` 연산자를 이용해 `newStr` 변수에 저장한 뒤 출력합니다.

## 깊게 들어가기

사실 `capitalize` 함수는 단순히 `toUpper` 함수만을 사용해도 충분히 구현할 수 있습니다. 예를 들어, `capitalize str = toUpper str`와 같이 작성할 수 있습니다. 왜냐하면 `Data.Char` 모듈에는 문자열을 한 글자씩 나눠주는 `words` 함수가 있기 때문입니다. `toUpper` 함수는 문자 하나를 인자로 받기 때문에, `words` 함수를 통해 문자열을 각각의 문자로 나눈 뒤 `toUpper` 함수를 적용하면 더 간결하게 문자열을 대문자로 바꿀 수 있습니다.

하지만 이런 접근 방식은 문자열을 한 글자씩 나누고 다시 붙이는 비효율적인 과정이 필요하기 때문에, 실제로는 `head`와 `tail` 함수를 사용하는 것이 더 좋은 방법입니다. 이렇게 하는 이유는 함수형 프로그래밍에서는 불필요한 작업을 최대한 줄이고 한 번에 한 가지 일만 수행하기 때문입니다.

## 참고

[Real World Haskell - Data.Char 모듈](http://book.realworldhaskell.org/read/characters-strings-and-escaping-rules.html#characters.intro)

[Kwon Yul의 Haskell Programming Language 소개 - 문자열 연산](https://kyagrd.github.io/haskell/haskell_intro.html#%EB%AC%B8%EC%9E%90%EC%97%B0%EC%82%B0)

## 더 보기