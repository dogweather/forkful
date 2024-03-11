---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:13.820630-07:00
description: "Haskell\uB85C \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD558\uB294 \uAC83\
  \uC740 \uD568\uC218\uAC00 \uC608\uC0C1\uB300\uB85C \uC791\uB3D9\uD558\uB294\uC9C0\
  \ \uC790\uB3D9\uD654\uB41C \uCCB4\uD06C\uB97C \uD1B5\uD574 \uBCF4\uC7A5\uD558\uB294\
  \ \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uBC84\uADF8\uB97C \uC870\uAE30\uC5D0 \uBC1C\
  \uACAC\uD558\uACE0, \uB9AC\uD329\uD1A0\uB9C1\uC744 \uC6A9\uC774\uD558\uAC8C \uD558\
  \uBA70, \uB3D9\uC791\uC744 \uBB38\uC11C\uD654\uD558\uC5EC \uCF54\uB4DC\uBCA0\uC774\
  \uC2A4\uB97C \uB354 \uC720\uC9C0\uBCF4\uC218\uD558\uAE30 \uC27D\uACE0 \uD655\uC7A5\
  \uC131 \uC788\uAC8C \uB9CC\uB4ED\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:29.221116-06:00'
model: gpt-4-0125-preview
summary: "Haskell\uB85C \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD558\uB294 \uAC83\uC740\
  \ \uD568\uC218\uAC00 \uC608\uC0C1\uB300\uB85C \uC791\uB3D9\uD558\uB294\uC9C0 \uC790\
  \uB3D9\uD654\uB41C \uCCB4\uD06C\uB97C \uD1B5\uD574 \uBCF4\uC7A5\uD558\uB294 \uAC83\
  \uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC774\uB97C \uD1B5\uD574 \uBC84\uADF8\uB97C \uC870\uAE30\uC5D0 \uBC1C\uACAC\
  \uD558\uACE0, \uB9AC\uD329\uD1A0\uB9C1\uC744 \uC6A9\uC774\uD558\uAC8C \uD558\uBA70\
  , \uB3D9\uC791\uC744 \uBB38\uC11C\uD654\uD558\uC5EC \uCF54\uB4DC\uBCA0\uC774\uC2A4\
  \uB97C \uB354 \uC720\uC9C0\uBCF4\uC218\uD558\uAE30 \uC27D\uACE0 \uD655\uC7A5\uC131\
  \ \uC788\uAC8C \uB9CC\uB4ED\uB2C8\uB2E4."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Haskell로 테스트를 작성하는 것은 함수가 예상대로 작동하는지 자동화된 체크를 통해 보장하는 것에 관한 것입니다. 프로그래머들은 이를 통해 버그를 조기에 발견하고, 리팩토링을 용이하게 하며, 동작을 문서화하여 코드베이스를 더 유지보수하기 쉽고 확장성 있게 만듭니다.

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
