---
changelog:
- 2024-01-30, dogweather, reviewed
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:21.685334-07:00
description: "\uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB294\uAC00: Haskell\uC740 \uB2E4\
  \uB978 \uC5B8\uC5B4\uB4E4\uCC98\uB7FC \uAE30\uBCF8\uC801\uC73C\uB85C \uC5F0\uAD00\
  \ \uBC30\uC5F4\uC744 \uC81C\uACF5\uD558\uC9C0 \uC54A\uC9C0\uB9CC, \uD0A4-\uAC12\
  \ \uC30D\uC744 \uB2E4\uB8E8\uAE30 \uC704\uD55C \uAC15\uB825\uD55C \uD45C\uC900 \uB77C\
  \uC774\uBE0C\uB7EC\uB9AC\uC778 `Data.Map`\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4.\
  \ \uC790, \uC774\uAC83\uC744 \uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB294\uC9C0 \uC54C\
  \uC544\uBD05\uC2DC\uB2E4! \uBA3C\uC800, \uC774\uAC83\uC744 \uC784\uD3EC\uD2B8\uD558\
  \uB294 \uAC83\uBD80\uD130 \uC2DC\uC791\uD558\uC138\uC694."
lastmod: '2024-03-13T22:44:55.285265-06:00'
model: gpt-4-0125-preview
summary: "Haskell\uC740 \uB2E4\uB978 \uC5B8\uC5B4\uB4E4\uCC98\uB7FC \uAE30\uBCF8\uC801\
  \uC73C\uB85C \uC5F0\uAD00 \uBC30\uC5F4\uC744 \uC81C\uACF5\uD558\uC9C0 \uC54A\uC9C0\
  \uB9CC, \uD0A4-\uAC12 \uC30D\uC744 \uB2E4\uB8E8\uAE30 \uC704\uD55C \uAC15\uB825\uD55C\
  \ \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC778 `Data.Map`\uC744 \uC81C\uACF5\
  \uD569\uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

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
