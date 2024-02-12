---
title:                "리팩토링"
aliases: - /ko/clojure/refactoring.md
date:                  2024-01-26T01:18:24.295876-07:00
model:                 gpt-4-0125-preview
simple_title:         "리팩토링"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/refactoring.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

리팩터링은 기존 컴퓨터 코드의 구조를 변경하지 않고 외부 동작을 변경하지 않는 과정으로, 비기능적 속성을 개선하는 것을 목표로 합니다. 프로그래머는 코드를 더 깨끗하고, 효율적이며, 유지 보수하기 쉽게 만들기 위해 리팩터링하여 소프트웨어의 가독성을 향상시키고 복잡성을 줄입니다.

## 방법:

클린한 문법과 함수형 패러다임 덕분에 Clojure에서의 리팩터링은 매우 직관적일 수 있습니다. 컬렉션 반복과 같은 일반적인 시나리오를 다뤄봅시다. 처음에는 `for` 루프로 시작할 수 있습니다:

```clojure
(defn calculate-sum [numbers]
  (reduce + 0 numbers))

(defn old-way []
  (let [nums (range 1 11)]
    (calculate-sum nums)))
```

`(old-way)`를 호출하면 1에서 10까지의 합인 55를 얻습니다. 하지만 이것을 더 Clojure스럽게 리팩터링 할 수 있습니다:

```clojure
(defn new-way []
  (->> (range 1 11)
       (reduce +)))
```

이 리팩터링된 `(new-way)` 함수는 스레딩 매크로를 사용하여 범위를 직접 `reduce`로 전달하여 불필요한 부분을 줄입니다.

## 깊이 들여다보기

리팩터링의 예술은 소프트웨어 개발 초기부터 뿌리를 두고 있지만, 특히 마틴 파울러의 1999년에 출판된 "Refactoring: Improving the Design of Existing Code"라는 기념비적인 책으로 크게 주목받았습니다. Clojure에서의 리팩터링은 종종 함수형 프로그래밍 원칙에 기대어, 순수 함수와 불변 데이터 구조를 선호합니다.

Clojure에서의 수동 리팩터링 대안으로는 Clojure에 특화된 자동 리팩터를 제공하는 인기 있는 IntelliJ IDEA 플러그인인 Cursive, 그리고 Clojure를 위한 Emacs 패키지인 clj-refactor가 있으며, 이는 다양한 리팩터링 기능을 제공합니다.

Clojure에서 리팩터링을 할 때 특별한 도전은 원칙적으로 불변하고 부작용이 없는 패러다임에서 상태와 부작용을 다루는 것입니다. 리팩터링 중 성능과 정확성을 유지하기 위해 atom, ref, agent, 그리고 transient의 신중한 사용이 중요합니다.

## 참고하기

- 마틴 파울러의 "Refactoring: Improving the Design of Existing Code" 기본 개념을 위해.
- [Clojure Docs](https://clojuredocs.org/) Clojure의 관용적인 코드 예시를 위해.
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) Emacs에서의 리팩터링 자동화를 위해.
- [Cursive](https://cursive-ide.com/) IntelliJ 사용자들이 자동 리팩터링 지원을 찾기 위해.
- [Refactoring with Rich Hickey](https://www.infoq.com/presentations/Simple-Made-Easy/) - 리팩터링에 관한 것은 아니지만, 효과적인 리팩터링 결정을 안내할 수 있는 Clojure 철학에 대한 통찰을 제공하는 Clojure의 창시자에 의한 강연입니다.
