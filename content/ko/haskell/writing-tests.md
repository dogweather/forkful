---
title:                "테스트 작성하기"
html_title:           "Haskell: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

테스트를 작성하는 것은 프로그래머들이 코드의 작동을 확인하고 버그를 발견하기 위해 하는 작업입니다. 테스트를 작성하면 코드의 동작을 검증하기 쉽고 안정적으로 유지할 수 있습니다.

## 진행 방법:

아래의 코드 블록들을 따라하며 테스트 작성 방법을 살펴보세요.

```Haskell
import Test.HUnit

-- 테스트하고자 하는 함수 작성
subtractTwo :: Int -> Int
subtractTwo x = x - 2

-- 테스트 케이스 작성
test1 = TestCase (assertEqual "substractTwo 5" 3 (subtractTwo 5))

-- 모든 테스트 케이스를 한 곳에 모은 후 실행
tests = TestList [TestLabel "test1" test1]

-- 실행
main = runTestTT tests
```

위의 코드를 실행하면 아래와 같은 결과를 볼 수 있습니다.

```Haskell
Cases: 1  Tried: 1  Errors: 0  Failures: 0

Counts {cases = 1, tried = 1, errors = 0, failures = 0}
```

## 더 깊게 살펴보기:

테스트 작성은 코드를 작성한 후에 이루어지는 중요한 단계입니다. 이전에는 테스트를 작성하기 위해 수작업으로 코드를 실행하고 결과를 확인하는 번거로운 작업이 필요했지만, 이제는 자동화된 테스트를 작성하여 더 효율적으로 코드를 검증할 수 있습니다. 또한 테스트 작성을 위해 사용할 수 있는 다양한 도구들이 존재하기 때문에 프로그래머들은 사용하기 편리한 도구를 선택하여 유니티하고 안정적인 코드 작성 환경을 구축할 수 있습니다.

## 관련 정보:

- [QuickCheck 개요](https://github.com/nick8325/quickcheck)
- [Tasty 개요](https://github.com/feuerbach/tasty)