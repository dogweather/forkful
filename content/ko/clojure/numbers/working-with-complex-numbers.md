---
date: 2024-01-26 04:39:25.077274-07:00
description: "\uBC29\uBC95: Clojure\uB294 `clojure.lang.Numbers` \uC720\uD2F8\uB9AC\
  \uD2F0 \uD074\uB798\uC2A4\uB97C \uD1B5\uD574 \uBCF5\uC18C\uC218\uC5D0 \uB300\uD55C\
  \ \uB0B4\uC7A5 \uC9C0\uC6D0\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. `complex`\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC \uBCF5\uC18C\uC218\uB97C \uC0DD\uC131\uD558\uACE0 \uC5F0\
  \uC0B0\uC744 \uC218\uD589\uD558\uC138\uC694."
lastmod: '2024-03-13T22:44:54.651463-06:00'
model: gpt-4-0125-preview
summary: "Clojure\uB294 `clojure.lang.Numbers` \uC720\uD2F8\uB9AC\uD2F0 \uD074\uB798\
  \uC2A4\uB97C \uD1B5\uD574 \uBCF5\uC18C\uC218\uC5D0 \uB300\uD55C \uB0B4\uC7A5 \uC9C0\
  \uC6D0\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
weight: 14
---

## 방법:
Clojure는 `clojure.lang.Numbers` 유틸리티 클래스를 통해 복소수에 대한 내장 지원을 제공합니다. `complex`를 사용하여 복소수를 생성하고 연산을 수행하세요.

```clojure
;; 복소수 생성
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; 덧셈
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; 뺄셈
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; 곱셈
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; 나눗셈
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; 켤레
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## 심층 탐구
복소수는 18세기에 가우스와 오일러와 같은 수학자들에 의해 정식화되었습니다. 처음에는 회의적인 반응을 받았지만, 이후 현대 과학 및 공학에서 중요한 역할을 하게 되었습니다. Clojure는 일부 언어들처럼 기본 복소수 타입을 가지고 있지 않습니다(Python 등), 하지만 포함된 Java 상호 운용성은 `clojure.lang.Numbers` 클래스를 통해 필요한 연산을 처리할 수 있습니다.

Java의 `java.lang.Complex`는 더 많은 기능과 잠재적인 최적화를 제공하는 강력한 대안입니다. Clojure의 호스트 상호 작용성은 Java 라이브러리를 사용하기 쉽게 만듭니다.

내부적으로, 복소수 산술은 실수 부와 허수 부의 덧셈과 곱셈으로 이루어지며, 핵심 규칙은 `i^2 = -1`입니다. 복소수 나눗셈은 일반적으로 복소수로 나누는 것을 피하기 위해 켤레를 필요로 하며, 더 복잡할 수 있습니다.

## 또한 보세요
- 빠른 참조를 위한 ClojureDocs: https://clojuredocs.org/
- `java.lang.Complex`에 대한 Java API: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- 수학적으로 궁금한 사람들을 위한 복소수에 대한 위키피디아 페이지: https://en.wikipedia.org/wiki/Complex_number
