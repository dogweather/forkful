---
title:    "Haskell: 테스트 작성하기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/writing-tests.md"
---

{{< edit_this_page >}}

# 왜

소프트웨어와 프로그래밍 분야에서 가장 중요한 것 중 하나는 코드를 테스트하는 것입니다. 왜냐하면 테스트를 통해 코드의 안정성과 신뢰성을 보장할 수 있기 때문입니다. 따라서 프로그래머나 개발자가 코드를 작성할 때 테스트를 케어하며 작업하는 것은 매우 중요합니다.

## 작성 방법

Haskell에서 테스트를 작성하는 방법은 매우 간단합니다. 먼저 테스트 프레임워크인 HUnit을 설치해야 합니다. 그리고 ```import Test.HUnit```을 통해 모듈을 불러오고, 테스트 케이스를 작성합니다. 일반적으로 각 함수마다 하나의 테스트 케이스를 작성하며, 이를 하나의 테스트 스위트로 묶어서 실행합니다.

예를 들어, 다음은 간단한 덧셈 함수를 테스트하는 코드입니다.

```Haskell
import Test.HUnit

-- 덧셈 함수 코드
add :: Int -> Int -> Int
add a b = a + b

-- 테스트 케이스
testAddition :: Test
testAddition = TestCase $ assertEqual "add 함수가 제대로 동작하지 않습니다." 10 (add 4 6)

-- 테스트 스위트
tests :: Test
tests = TestList [TestLabel "덧셈 함수 테스트" testAddition]

-- 실행
main :: IO ()
main = runTestTT tests
```

위 코드를 실행하면 콘솔에 테스트 결과가 나타납니다. 그 결과는 다음과 같습니다.

```
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Counts {cases = 1, tried = 1, errors = 0, failures = 0}

Finished in 0.0001 seconds
```

만약 테스트 케이스가 실패하는 경우, 이를 표시해줍니다. 따라서 위와 같이 제대로 동작하는 경우 모든 테스트 케이스가 통과하며 콘솔에 결과가 출력됩니다.

## 깊이있는 탐구

테스트를 작성할 때 주의해야 할 몇 가지 사항이 있습니다. 첫째, 테스트의 범위는 충분히 넓어야 합니다. 너무 작은 단위의 테스트는 코드를 신뢰할 수 없게 만들 수 있습니다. 또한 각 테스트 케이스는 독립적이어야 합니다. 한 테스트 케이스가 다른 테스트 케이스에 영향을 주거나 의존하지 않도록 작성해야 합니다.

둘째, 어떤 시나리오에서도 코드가 일관되게 동작하도록 여러 테스트 케이스를 작성해야 합니다. 특히 예외 상황에 대한 테스트를 꼭 포함하도록 주의해야 합니다. 예외 상황에서도 코드가 제대로 동작하도록 테스트해야 실제로 사용할 때 불안정한 동작을 방지할 수 있습니다.

마지막으로, 테스트 코드도 역시 관리되어야 합니다. 이는 일반적으로 작성한 코드보다 더 중요한 부분입니다. 테스트 코드는 만들어진 코드가 변경되면 함께 변경되어야 하며, 필요한 경우 테스트 코드를 수정해야 합니다.

# 더 알아보기

- [HUnit 사용