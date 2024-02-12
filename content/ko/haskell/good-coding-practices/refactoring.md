---
title:                "리팩토링"
aliases: - /ko/haskell/refactoring.md
date:                  2024-01-26T01:18:48.891605-07:00
model:                 gpt-4-0125-preview
simple_title:         "리팩토링"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/refactoring.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
리팩터링은 코드의 외부 동작을 변경하지 않고 코드를 조정하는 과정입니다. 코드를 더 쉽게 읽고, 관리하며, 확장할 수 있도록 정리하고 조직하는 것이 전부입니다. 또한 버그를 제거하고 성능을 향상시키는 데도 도움이 될 수 있습니다.

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
