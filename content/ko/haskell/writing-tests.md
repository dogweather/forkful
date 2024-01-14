---
title:                "Haskell: 테스트 작성하기"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## 왜 테스트를 작성하는가

소프트웨어 개발에서 테스트는 매우 중요합니다. 테스트를 작성하는 것은 코드의 품질과 안정성을 보증하고 결함을 발견하는 데 도움이 됩니다. 이를 통해 유지보수가 더욱 쉬워지고 프로젝트의 성공 확률이 높아집니다.

## 어떻게 작성하는가

가장 대표적인 함수형 프로그래밍 언어인 Haskell에서 코드를 테스트하는 방법을 알아보겠습니다. 먼저 `hspec` 패키지를 사용하여 간단한 테스트 프레임워크를 만들어보겠습니다. 코드 블록은 아래와 같이 작성할 수 있습니다.

```Haskell
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "더하기 함수" $ do
        it "1 더하기 1은 2를 출력해야 합니다." $
            1 + 1 `shouldBe` 2
```

위의 예제에서 `describe`와 `it`은 테스트를 조직화하는 데 사용되는 함수입니다. `shouldBe`는 두 값이 같은지 여부를 비교하고 다르면 테스트를 실패시킵니다. 위의 코드를 실행하면 테스트가 통과하지 못하며, 아래와 같은 결과를 얻을 수 있습니다.

```Haskell
더하기 함수
  [x] 1 더하기 1은 2를 출력해야 합니다.
```

## 더 깊이 있게

Haskell에서는 `QuickCheck`이라는 패키지를 사용하여 프로퍼티 기반 테스트를 작성할 수도 있습니다. 이를 사용하면 무작위로 생성한 입력 값을 테스트 함수에 전달하고 결과가 기대하는 프로퍼티를 충족하는지 검사할 수 있습니다. 예를 들어, 다음과 같은 코드를 작성할 수 있습니다.

```Haskell
import Test.QuickCheck

prop_reverse :: [Int] -> Bool
prop_reverse list = reverse (reverse list) == list

main :: IO ()
main = quickCheck prop_reverse
```

위의 예제에서는 어떤 리스트를 뒤집어도 원래 리스트와 같은지 검사하는 프로퍼티를 정의하고, `quickCheck`를 통해 테스트를 실행합니다. 만약 프로퍼티를 만족하지 않는 입력 값이 생성되면 테스트는 실패하게 됩니다.

## 더 알아보기

더 많은 테스트 방법과 패키지를 알아보려면 아래의 링크를 참고해보세요.

## 참고하기

- [Hspec 패키지 문서](https://hackage.haskell.org/package/hspec)
- [QuickCheck 패키지 문서](https://hackage.haskell.org/package/QuickCheck)
- [Haskell 테스트를 위한 패키지 목록](https://wiki.haskell.org/Testing_packages)