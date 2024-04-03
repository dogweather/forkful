---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:13.820630-07:00
description: "\uBC29\uBC95: Haskell\uC740 \uC5EC\uB7EC \uD14C\uC2A4\uD305 \uD504\uB808\
  \uC784\uC6CC\uD06C\uB97C \uC9C0\uC6D0\uD558\uC9C0\uB9CC, `Hspec`\uACFC `QuickCheck`\uC774\
  \uB77C\uB294 \uB450 \uAC00\uC9C0 \uC778\uAE30 \uC788\uB294 \uD504\uB808\uC784\uC6CC\
  \uD06C\uAC00 \uC788\uC2B5\uB2C8\uB2E4. Hspec\uC744 \uC0AC\uC6A9\uD558\uBA74 \uCF54\
  \uB4DC\uC5D0 \uB300\uD55C \uC778\uAC04\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uC0AC\
  \uC591\uC744 \uC815\uC758\uD560 \uC218 \uC788\uACE0, QuickCheck\uC744 \uC0AC\uC6A9\
  \uD558\uBA74 \uCF54\uB4DC\uAC00 \uCDA9\uC871\uD574\uC57C\u2026"
lastmod: '2024-03-13T22:44:55.300297-06:00'
model: gpt-4-0125-preview
summary: "Haskell\uC740 \uC5EC\uB7EC \uD14C\uC2A4\uD305 \uD504\uB808\uC784\uC6CC\uD06C\
  \uB97C \uC9C0\uC6D0\uD558\uC9C0\uB9CC, `Hspec`\uACFC `QuickCheck`\uC774\uB77C\uB294\
  \ \uB450 \uAC00\uC9C0 \uC778\uAE30 \uC788\uB294 \uD504\uB808\uC784\uC6CC\uD06C\uAC00\
  \ \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 방법:
Haskell은 여러 테스팅 프레임워크를 지원하지만, `Hspec`과 `QuickCheck`이라는 두 가지 인기 있는 프레임워크가 있습니다. Hspec을 사용하면 코드에 대한 인간이 읽을 수 있는 사양을 정의할 수 있고, QuickCheck을 사용하면 코드가 충족해야 하는 속성을 설명함으로써 테스트를 자동으로 생성할 수 있습니다.

### Hspec 사용하기
먼저, 빌드 도구 설정(`stack.yaml` 또는 `cabal` 파일 등)에 `hspec`을 추가합니다. 그런 다음, `Test.Hspec`을 임포트하고 테스트를 사양으로 작성합니다:

```haskell
-- file: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "두 숫자를 더한다" $
    add 1 2 `shouldBe` 3

  it "두 번째 숫자로 제로를 더하면 첫 번째 숫자를 반환한다" $
    add 5 0 `shouldBe` 5
```

그런 다음, 빌드 도구를 사용하여 테스트를 실행하면 다음과 같은 출력이 나타날 수 있습니다:

```
MyLib.add
  - 두 숫자를 더한다
  - 두 번째 숫자로 제로를 더하면 첫 번째 숫자를 반환한다

0.0001초에 완료
2개의 예제, 0개의 실패
```

### QuickCheck 사용하기
QuickCheck을 사용하면 함수가 충족해야 하는 속성을 표현합니다. 프로젝트 설정에 `QuickCheck`을 추가한 후, 이를 임포트합니다:

```haskell
-- file: test/MyLibProperties.hs
import Test.QuickCheck
import MyLib (add)

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = x + (y + z) == (x + y) + z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck prop_addAssociative
  quickCheck prop_addCommutative
```

이 테스트를 실행하면 지정된 속성을 확인하기 위해 입력을 자동으로 생성합니다:

```
+++ OK, 100회 테스트를 통과함.
+++ OK, 100회 테스트를 통과함.
```

Hspec과 QuickCheck 예제 모두에서 테스트 스위트는 코드의 정확성을 자동으로 검증할 수 있는 실행 가능한 문서로 기능합니다.
