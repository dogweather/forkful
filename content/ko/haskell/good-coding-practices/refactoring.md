---
date: 2024-01-26 01:18:48.891605-07:00
description: "\uB9AC\uD329\uD130\uB9C1\uC740 \uCF54\uB4DC\uC758 \uC678\uBD80 \uB3D9\
  \uC791\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uCF54\uB4DC\uB97C \uC870\uC815\
  \uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uCF54\uB4DC\uB97C \uB354 \uC27D\uAC8C\
  \ \uC77D\uACE0, \uAD00\uB9AC\uD558\uBA70, \uD655\uC7A5\uD560 \uC218 \uC788\uB3C4\
  \uB85D \uC815\uB9AC\uD558\uACE0 \uC870\uC9C1\uD558\uB294 \uAC83\uC774 \uC804\uBD80\
  \uC785\uB2C8\uB2E4. \uB610\uD55C \uBC84\uADF8\uB97C \uC81C\uAC70\uD558\uACE0 \uC131\
  \uB2A5\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uB294 \uB370\uB3C4 \uB3C4\uC6C0\uC774 \uB420\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.307731-06:00'
model: gpt-4-0125-preview
summary: "\uB9AC\uD329\uD130\uB9C1\uC740 \uCF54\uB4DC\uC758 \uC678\uBD80 \uB3D9\uC791\
  \uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uCF54\uB4DC\uB97C \uC870\uC815\uD558\
  \uB294 \uACFC\uC815\uC785\uB2C8\uB2E4."
title: "\uB9AC\uD329\uD1A0\uB9C1"
weight: 19
---

## 방법:
여러분이 자신의 좋아하는 노래만큼이나 반복되는 Haskell 코드 덩어리를 가지고 있다고 가정해 봅시다. 여기 함수를 사용하여 리팩터링할 수 있는 방법을 빠르게 살펴보겠습니다.

리팩터링 전:

```haskell
printInvoice :: String -> Float -> String -> IO ()
printInvoice customer total item = do
  putStrLn $ "Customer: " ++ customer
  putStrLn $ "Total: " ++ show total
  putStrLn $ "Item: " ++ item
```

리팩터링 후 조금:

```haskell
printDetail :: String -> String -> IO ()
printDetail label value = putStrLn $ label ++ ": " ++ value

printInvoice :: String -> Float -> String -> IO ()
printInvoice customer total item = do
  printDetail "Customer" customer
  printDetail "Total" (show total)
  printDetail "Item" item

-- 샘플 출력:
-- Customer: Alice
-- Total: $42.00
-- Item: Haskell 프로그래밍 가이드
```

볼 수 있듯이, 공통 패턴을 별도의 `printDetail` 함수로 추출함으로써 반복을 피하고 `printInvoice`를 더 명확하고 관리하기 쉽게 만듭니다.

## 깊이 들어가기
1980년대 후반 Haskell이 등장했을 때, 함수형 패러다임이 코딩 관행에 새로운 바람을 불러일으킬 수 있음이 분명했습니다. 시간이 흘러, 함수가 일등 시민이며 강력한 정적 타입 시스템을 가진 Haskell에서의 리팩터링은 특히 우아합니다. 컴파일러가 여러분의 등을 지켜주기 때문에 앱을 망가뜨릴까 봐 두려워할 필요 없이 리팩터링을 할 수 있습니다.

수동 리팩터링 대안으로는 자동화된 도구를 사용하는 것이 포함될 수 있지만, Haskell의 함수형 특성과 타입 안전성 때문에 다른 언어에 비해 이것이 덜 만연할 수 있습니다. 구현 측면에서는, 리팩터링을 더욱 원활하게 하기 위해 Haskell의 고차 함수, 순수성, 불변성과 같은 기능을 활용하는 것이 중요합니다.

방금 보여준 "함수 추출"과 같은 리팩터링은 일반적이지만, 타입 시스템 덕분에 "함수 인라인화", "변수 이름 변경", "함수 서명 변경"도 자신 있게 할 수 있습니다. Haskell의 강력한 타입 추론은 다른 언어에서는 발생할 수 있는 오류를 가끔씩 잡아낼 수 있습니다.

## 참고하기
Haskell에서의 리팩터링을 깊이 파고들고 싶다면, Martin Fowler의 "Refactoring: Improving the Design of Existing Code"에 나온 개념이 보편적으로 적용될 수 있다는 점을 확인해 보세요. Haskell 코드를 개선하기 위한 자동 힌트에 대해 hlint 도구를 확인해 보세요. 또한, 커뮤니티 통찰과 추가 읽을거리를 위해 Haskell 위키(https://wiki.haskell.org/Refactoring)를 방문해 보세요.
