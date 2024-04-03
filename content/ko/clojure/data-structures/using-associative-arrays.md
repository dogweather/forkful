---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:23.037027-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: \uD074\uB85C\uC800\uC5D0\uC11C \uC5F0\uAD00\
  \ \uBC30\uC5F4(\uD574\uC2DC \uB9F5)\uC744 \uC0DD\uC131\uD558\uACE0 \uC870\uC791\uD558\
  \uB294 \uAC83\uC740 \uAC04\uB2E8\uD569\uB2C8\uB2E4. \uC608\uC81C\uB97C \uD1B5\uD574\
  \ \uC0B4\uD3B4\uBCF4\uACA0\uC2B5\uB2C8\uB2E4. \uD574\uC2DC \uB9F5 \uC0DD\uC131."
lastmod: '2024-03-13T22:44:54.650023-06:00'
model: gpt-4-0125-preview
summary: "\uD074\uB85C\uC800\uC5D0\uC11C \uC5F0\uAD00 \uBC30\uC5F4(\uD574\uC2DC \uB9F5\
  )\uC744 \uC0DD\uC131\uD558\uACE0 \uC870\uC791\uD558\uB294 \uAC83\uC740 \uAC04\uB2E8\
  \uD569\uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

## 사용 방법:
클로저에서 연관 배열(해시 맵)을 생성하고 조작하는 것은 간단합니다. 예제를 통해 살펴보겠습니다.

해시 맵 생성:

```clojure
(def my-map {:name "Alex" :age 30})
```

키를 명시하여 값을 검색할 수 있습니다:

```clojure
(get my-map :name)
;; "Alex"
```
또는, 보다 관용적으로 키를 함수로 사용할 수 있습니다:

```clojure
(:name my-map)
;; "Alex"
```

항목 추가 또는 업데이트가 간단합니다:

```clojure
(def updated-map (assoc my-map :location "New York"))
;; {:name "Alex", :age 30, :location "New York"}

(def incremented-age (update my-map :age inc))
;; {:name "Alex", :age 31}
```

키를 제거하려면 `dissoc`을 사용합니다:

```clojure
(def removed-age (dissoc my-map :age))
;; {:name "Alex"}
```

맵을 반복하기 위해서는:

```clojure
(doseq [[k v] my-map] (println k "->" v))
;; :name -> Alex
;; :age -> 30
```

조건부 접근을 위해, `find`는 키가 존재하면 키-값 쌍을 반환합니다:

```clojure
(find my-map :age)
;; [:age 30]
```

## 심화 탐구
클로저에서의 연관 배열, 일반적으로 해시 맵으로도 알려져 있으며, 키-값 기반 데이터를 관리하기 위해 매우 다용도적이고 효율적입니다. 변경 불가능성과 함수형 프로그래밍의 언어 철학에 깊이 뿌리박고 있는 클로저의 풍부한 컬렉션 라이브러리의 일부입니다. 요소에 접근하기 위해 O(n) 시간 복잡도가 필요한 배열이나 목록과 달리, 해시 맵은 접근에 대해 거의 상수 시간 복잡도를 제공하여 검색 작업에 매우 효율적입니다.

인덱싱된 접근을 통해 클로저의 벡터가 유사한 목적을 제공할 수 있다고 주장할 수 있지만, 해시 맵은 키가 의미 있는 설명자를 제공하는 비 순차적이고 라벨이 붙은 데이터를 다룰 때 빛을 발합니다.

클로저(그리고 그것의 리스프 유산)에 특별한 것은, 연관 배열이 일급 시민이라는 점입니다. 즉, 특별한 문법이나 접근 방법 없이도 직접 조작하고 함수 주위로 전달할 수 있습니다. 이 설계 결정은 클로저의 단순성과 강력함에 대한 강조를 강화합니다.

해시 맵이 매우 유용함에도 불구하고, 매우 큰 데이터셋이나 키가 매우 동적인 (지속적인 추가와 제거) 상황의 경우, 대안적인 데이터 구조나 데이터베이스가 더 나은 성능과 유연성을 제공할 수 있습니다. 그러나 클로저 애플리케이션의 영역 내 대부분의 전형적인 사용 사례를 위해, 연관 배열은 데이터 관리를 위한 견고하고 효율적인 수단을 제공합니다.
