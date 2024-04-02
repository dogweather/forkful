---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:01.974127-07:00
description: "\uC5F0\uAD00 \uBC30\uC5F4, \uB610\uB294 Elm\uC5D0\uC11C \uBD80\uB974\
  \uB294 \uB300\uB85C, \uC0AC\uC804\uC740 \uD0A4\uC640 \uAC12 \uC0AC\uC774\uC758 \uB9E4\
  \uD551\uC744 \uC81C\uACF5\uD558\uC5EC \uAC12\uC744 \uCC3E\uC544\uBCF4\uACE0, \uC0BD\
  \uC785\uD558\uACE0, \uC0AD\uC81C\uD558\uB294 \uAC83\uC744 \uB9E4\uC6B0 \uBE60\uB974\
  \uAC8C \uB9CC\uB4E4\uC5B4\uC90D\uB2C8\uB2E4. \uC0AC\uC6A9\uC790 \uC124\uC815\uC774\
  \uB098 \uC778\uBCA4\uD1A0\uB9AC \uBAA9\uB85D\uACFC \uAC19\uC774 \uC5C4\uACA9\uD55C\
  \ \uC21C\uC11C\uB97C \uC694\uAD6C\uD558\uC9C0 \uC54A\uB294 \uAC83\uB4E4\uC744 \uCD94\
  \uC801\uD574\uC57C \uD560 \uB54C \uC0AC\uC6A9\uD558\uBA74 \uC88B\uC2B5\uB2C8\uB2E4\
  ."
lastmod: '2024-03-13T22:44:55.100642-06:00'
model: gpt-4-0125-preview
summary: "\uC5F0\uAD00 \uBC30\uC5F4, \uB610\uB294 Elm\uC5D0\uC11C \uBD80\uB974\uB294\
  \ \uB300\uB85C, \uC0AC\uC804\uC740 \uD0A4\uC640 \uAC12 \uC0AC\uC774\uC758 \uB9E4\
  \uD551\uC744 \uC81C\uACF5\uD558\uC5EC \uAC12\uC744 \uCC3E\uC544\uBCF4\uACE0, \uC0BD\
  \uC785\uD558\uACE0, \uC0AD\uC81C\uD558\uB294 \uAC83\uC744 \uB9E4\uC6B0 \uBE60\uB974\
  \uAC8C \uB9CC\uB4E4\uC5B4\uC90D\uB2C8\uB2E4. \uC0AC\uC6A9\uC790 \uC124\uC815\uC774\
  \uB098 \uC778\uBCA4\uD1A0\uB9AC \uBAA9\uB85D\uACFC \uAC19\uC774 \uC5C4\uACA9\uD55C\
  \ \uC21C\uC11C\uB97C \uC694\uAD6C\uD558\uC9C0 \uC54A\uB294 \uAC83\uB4E4\uC744 \uCD94\
  \uC801\uD574\uC57C \uD560 \uB54C \uC0AC\uC6A9\uD558\uBA74 \uC88B\uC2B5\uB2C8\uB2E4\
  ."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

## 무엇 & 왜?

연관 배열, 또는 Elm에서 부르는 대로, 사전은 키와 값 사이의 매핑을 제공하여 값을 찾아보고, 삽입하고, 삭제하는 것을 매우 빠르게 만들어줍니다. 사용자 설정이나 인벤토리 목록과 같이 엄격한 순서를 요구하지 않는 것들을 추적해야 할 때 사용하면 좋습니다.

## 사용 방법:

Elm에서는 `Dict` 모듈을 통해 사전을 다룹니다. 간단한 예제로 시작해봅시다:

```Elm
import Dict exposing (Dict)

-- 문자열 키와 정수 값으로 사전 초기화하기
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- 값 추가 또는 업데이트하기
updatedDict = Dict.insert "grape" 10 exampleDict

-- 값을 검색하기 (키가 존재하지 않을 수도 있으므로 Maybe 타입에 주목하세요)
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- 키-값 쌍 제거하기
finalDict = Dict.remove "banana" updatedDict

-- 사전을 다시 리스트로 변환하기
dictToList = Dict.toList finalDict
```

`dictToList`를 표시할 때의 샘플 출력:

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

이것은 사전을 생성하고, 업데이트하고, 접근하고, 순회하는 기본적인 작업을 보여줍니다.

## 심화 학습

Elm의 사전은 내부적으로 AVL 트리라고 알려진 구조를 사용합니다 - 자체 균형 이진 검색 트리의 한 유형입니다. 이 선택은 삽입, 검색, 삭제와 같은 작업의 성능(로그 시간 복잡도)을 보장하면서도 데이터를 다루는 것을 간단하게 유지하는 사이의 균형을 맞춥니다.

Elm의 `Dict`의 장점에도 불구하고, 모든 경우에 일반적으로 적합한 해결책은 아닙니다. 순서가 있거나 순차적으로 순회해야 하는 컬렉션의 경우, 리스트 또는 배열이 더 적합할 수 있습니다. 또한, 알려진 키 세트를 다룰 때, 사용자 정의 타입(Elm의 열거형 버전)을 사용하면 코드에서 더 많은 타입 안전성과 명확한 의도를 제공할 수 있습니다.

Elm 생태계에서, `Dict`는 키가 고유하고 순서가 중요하지 않은 키-값 쌍의 컬렉션을 관리하는 신뢰할 수 있는 방법을 제공합니다. 더 새롭거나 더 정교한 구조가 등장할 수도 있지만, 연관 배열을 다루는 단순성과 효율성 때문에 `Dict` 모듈은 Elm 프로그래머의 도구 상자에서 기본적인 도구로 남아 있습니다.
