---
title:                "테스트 작성하기"
date:                  2024-01-19
html_title:           "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
테스트 작성은 코드가 예상대로 작동하는지 확인하기 위한 과정입니다. 프로그래머들은 버그를 미리 찾아내고, 나중에 코드 변경 시 안정성을 보장하기 위해 테스트를 합니다.

## How to: (어떻게 하나?)
Haskell에서 테스트는 주로 Hspec과 QuickCheck 라이브러리를 사용해 작성합니다. 간단한 예시로 함수의 올바른 실행을 확인해봅시다.

```Haskell
-- simpleFunction.hs
module SimpleFunction where

add :: Int -> Int -> Int
add x y = x + y
```

테스트를 위해 Hspec을 사용할 예제 코드입니다:

```Haskell
-- simpleFunctionTest.hs
import Test.Hspec
import SimpleFunction

main :: IO ()
main = hspec $ do
  describe "add function" $ do
    it "correctly adds two numbers" $ do
      add 2 3 `shouldBe` 5
```

실행 결과는 다음과 같습니다:

```
add function
  - correctly adds two numbers

Finished in 0.0001 seconds
1 example, 0 failures
```

## Deep Dive (깊이 파기)
Haskell의 테스트는 주로 두 가지 라이브러리로 구성됩니다: Hspec과 QuickCheck. Hspec은 BDD(Behavior-Driven Development) 스타일의 테스트를 가능하게 하며, QuickCheck은 프로퍼티 기반 테스팅을 제공하여 무작위 데이터로 테스트를 수행합니다. 이 두 라이브러리는 2000년대 초반에 개발되었으며, 타입 안정성과 함수의 순수성을 강조하는 Haskell의 철학을 반영합니다. 테스트 작성 시, 모나드의 사용이나 순수 함수의 특성에 주의를 기울여야 합니다.

## See Also (추가 자료)
- [Hspec User Manual](http://hspec.github.io/)
- [Real World Haskell - Testing and quality assurance](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html)
- [Stackage: A Haskell Package Repository](https://www.stackage.org/)
- [Haskell Wiki on Testing](https://wiki.haskell.org/Testing)
