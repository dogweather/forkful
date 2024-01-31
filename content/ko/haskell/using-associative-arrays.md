---
title:                "연관 배열 사용하기"
date:                  2024-01-30T19:11:21.685334-07:00
model:                 gpt-4-0125-preview
simple_title:         "연관 배열 사용하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/using-associative-arrays.md"
changelog:
  - 2024-01-30, dogweather, reviewed
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?

Haskell에서의 연관 배열 또는 딕셔너리는 키와 값을 매핑하여 빠른 조회와 효율적인 데이터 관리를 위한 것입니다. 프로그래머들은 요소들의 집합을 다루는 데 있어서, 리스트에 비해 원소를 검색하는 것이 훨씬 수월하기 때문에 이를 사용합니다.

## 어떻게 사용하는가:

Haskell은 다른 언어들처럼 기본적으로 연관 배열을 제공하지 않지만, 키-값 쌍을 다루기 위한 강력한 표준 라이브러리인 `Data.Map`을 제공합니다. 자, 이것을 어떻게 사용하는지 알아봅시다!

먼저, 이것을 임포트하는 것부터 시작하세요:
```Haskell
import qualified Data.Map as Map
```

맵을 생성하는 것은 간단합니다. 프로그래밍 언어와 그 패러다임으로 구성된 맵을 생성해 봅시다:
```Haskell
let languages = Map.fromList [("Haskell", "Functional"), ("Python", "Imperative"), ("Prolog", "Logical")]
```

이제 Haskell의 패러다임을 어떻게 얻을까요?
```Haskell
Map.lookup "Haskell" languages
-- 출력: Just "Functional"
```

새로운 언어를 추가하는 것은 쉽습니다:
```Haskell
let languagesUpdated = Map.insert "Rust" "Systems" languages
```

모든 언어를 나열하고 싶다면? `Map.keys`를 사용하세요:
```Haskell
Map.keys languagesUpdated
-- 출력: ["Haskell","Python","Prolog","Rust"]
```

패러다임을 나열하기 위해서는 `Map.elems`을 사용하세요:
```Haskell
Map.elems languagesUpdated
-- 출력: ["Functional","Imperative","Logical","Systems"]
```

이러한 기본적인 작업들은 대부분의 용도를 커버할 것이지만, `Data.Map`에서 탐색할 내용은 이보다 훨씬 더 많습니다!

## 심층 분석

Haskell 표준 라이브러리의 `Data.Map` 모듈은 균형 잡힌 이진 트리, 구체적으로는 AVL 트리를 기반으로 구축되었습니다. 이 선택은 삽입, 삭제, 조회와 같은 맵 위의 대부분의 작업을 O(log n) 시간에 할 수 있게 해주며, 여기서 n은 맵에 있는 요소의 수입니다. 이는 많은 사용 사례에 대해 효율적인 선택이지만, 모든 시나리오에 대해 가장 빠른 것은 아닙니다.

역사적 뉘앙스도 있습니다: `Data.Map`이 널리 쓰이기 전에, Haskell 프로그래머들은 종종 연관 배열을 모방하기 위해 쌍의 리스트를 사용했습니다. 그러나, 이러한 구조에서의 작업들은 조회에 O(n)이 걸리므로, 성능 측면에서 볼 때 `Data.Map`은 상당한 개선입니다.

이제, `Data.Map`의 효율성과 유용성에도 불구하고, 모든 작업에 가장 좋은 도구는 아닙니다. O(log n)의 조회 시간마저도 너무 느린, 고성능을 요구하는 작업이나 키가 항상 정수 값인 경우에는 배열이나 해시 테이블(`Data.HashMap`을 통해)이 O(1) 접근 시간으로 더 나은 성능을 제공할 수 있습니다.

Haskell 생태계는 다양한 요구를 충족시키기 위한 다양한 데이터 구조를 허용하며, `Data.Map`은 사용의 용이성, 유연성 및 성능의 균형을 맞춘 연관 배열을 위한 훌륭한 일반적인 선택입니다.
