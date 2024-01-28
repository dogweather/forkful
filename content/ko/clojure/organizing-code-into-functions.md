---
title:                "코드를 함수로 구성하기"
date:                  2024-01-26T01:09:38.384186-07:00
model:                 gpt-4-1106-preview
simple_title:         "코드를 함수로 구성하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 무엇이며 왜하는가?

코드를 함수로 조각내는 것은 특정 작업을 수행하는 코드 블록을 패키징하는 것에 관한 것입니다. 이러한 작업을 함으로써 코드가 깨끗해지며, 유지보수가 쉬워지고, 다른 개발자들이 읽기에도 수월해집니다.

## 방법:

클로저(Clojure)에서 함수는 `defn`을 이용해 정의되며, 그 뒤에 이름, 파라미터, 그리고 몸체가 옵니다. 아래는 간단한 예시입니다.

```Clojure
(defn greet [name]
  (str "Hello, " name "!"))

(greet "Alex") ; => "Hello, Alex!"
```

이제 직사각형의 면적을 계산하고 싶다고 해 봅시다. 모든 것을 한꺼번에 섞는 대신, 두 개의 함수로 나눕니다:

```Clojure
(defn area [length width]
  (* length width))

(defn print-area [length width]
  (println "The area is:" (area length width)))

(print-area 3 4) ; => The area is: 12
```

## 깊이 있는 탐구

옛날에는 프로그래머들이 모든 로직을 단일 블록 안에 그대로 밀어 넣었습니다. 보기에도 안 좋았죠. 그러다 구조적 프로그래밍이 등장하면서 함수가 일반적인 것이 되었습니다. 클로저에서는 모든 함수가 일급(first-class)입니다—당신은 다른 어떤 값들처럼 자유롭게 함수를 사용할 수 있습니다.

대안들? 어떤 사람들은 멀티-메소드나 고차 함수를 사용할 수 있지만, 그것들은 함수 스튜 속의 양념일 뿐입니다.

함수의 모든 세부사항이 중요합니다: 클로저에서 함수는 변경 불가능합니다, 이로 인해 부작용 문제가 덜 발생합니다. 또한 전형적인 루프 대신에 재귀에 기대는 것이 클로저의 함수형 패러다임과 잘 어울립니다.

## 참고하면 좋을 자료들

- 클로저 자체 가이드: https://clojure.org/guides/learn/functions
- 함수형 프로그래밍 기초: https://www.braveclojure.com/core-functions-in-depth/
- 리치 히키의 강연들: https://changelog.com/posts/rich-hickeys-greatest-hits - 클로저의 철학에 대한 통찰을 얻기 위해.
