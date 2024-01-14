---
title:                "Haskell: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## 왜?


코드를 배포하기 전에 테스트를 작성하는 것은 매우 중요합니다. 이를 통해 코드의 문제점을 발견할 수 있고 더 나은 품질의 소프트웨어를 제공할 수 있습니다. 또한 코드의 변경 사항에 대한 자신감을 가질 수 있습니다.

## 한 번에 봐요!

```Haskell
-- 테스트를 위한 모듈을 임포트합니다.
import Test.HUnit

-- 간단한 함수 예제
-- 숫자 x와 y를 더하는 함수
add :: Int -> Int -> Int
add x y = x + y

-- add 함수를 테스트합니다.
addTest :: Test
addTest = TestList [
    TestCase (assertEqual "1 + 2 is 3" 3 (add 1 2)),
    TestCase (assertEqual "-1 + 4 is 3" 3 (add (-1) 4))
    -- 다른 테스트 케이스를 추가해보세요!
    ]

-- 모든 테스트 케이스를 실행합니다.
main :: IO ()
main = do
    runTestTT addTest
```

위의 코드는 `Test.HUnit` 모듈을 사용하여 `add` 함수를 테스트하는 예제입니다. `assertEqual` 함수를 사용하여 원하는 출력과 실제 출력이 동일한지를 비교합니다. 이와 같이 테스트를 작성하면 코드의 기능을 잘 이해할 수 있고 오류가 발생했을 때 쉽게 찾을 수 있습니다.

## 더 깊이 들어가보세요

테스트를 작성할 때 다양한 방법과 라이브러리를 사용할 수 있습니다. 가장 기본적인 방법은 위에서 보여준 것처럼 `assertEqual`을 사용하는 것입니다. 그러나 더 나은 오류 메시지와 편리한 문법을 제공하는 `Hspec`이나 테스트를 더 쉽게 작성할 수 있는 `QuickCheck`등 다양한 옵션이 있습니다. 또한 테스트를 구성하는 방법과 효율적인 테스트 작성 방법에 대해서도 더 많이 배울 수 있습니다.

## 더 알아보기

[공식 HUnit 문서](https://hackage.haskell.org/package/HUnit)    
[Hspec 라이브러리](https://hspec.github.io/)    
[QuickCheck 라이브러리](https://hackage.haskell.org/package/QuickCheck)