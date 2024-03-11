---
date: 2024-01-26 04:39:25.077274-07:00
description: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218\uC5D0 \uCD94\uAC00\uC801\uC778\
  \ \uBD80\uBD84, \uC989 \uD5C8\uC218 \uB2E8\uC704 'i'\uB97C \uD3EC\uD568\uD558\uC5EC\
  \ \uC2E4\uC218\uB97C \uD655\uC7A5\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC2E0\uD638 \uCC98\uB9AC, \uC804\uC790\uAE30 \uC774\uB860, \uD504\uB799\
  \uD0C8 \uB4F1\uC758 \uBD84\uC57C\uC5D0\uC11C \uC74C\uC218\uC758 \uC81C\uACF1\uADFC\
  \uC744 \uD3EC\uD568\uD558\uB294 \uACC4\uC0B0\uC774 \uC77C\uC0C1\uC801\uC73C\uB85C\
  \ \uD544\uC694\uD55C \uB2E4\uC591\uD55C \uB3C4\uBA54\uC778\uC5D0\uC11C \uBCF5\uC18C\
  \uC218\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:28.566984-06:00'
model: gpt-4-0125-preview
summary: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218\uC5D0 \uCD94\uAC00\uC801\uC778 \uBD80\
  \uBD84, \uC989 \uD5C8\uC218 \uB2E8\uC704 'i'\uB97C \uD3EC\uD568\uD558\uC5EC \uC2E4\
  \uC218\uB97C \uD655\uC7A5\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC2E0\uD638 \uCC98\uB9AC, \uC804\uC790\uAE30 \uC774\uB860, \uD504\uB799\uD0C8\
  \ \uB4F1\uC758 \uBD84\uC57C\uC5D0\uC11C \uC74C\uC218\uC758 \uC81C\uACF1\uADFC\uC744\
  \ \uD3EC\uD568\uD558\uB294 \uACC4\uC0B0\uC774 \uC77C\uC0C1\uC801\uC73C\uB85C \uD544\
  \uC694\uD55C \uB2E4\uC591\uD55C \uB3C4\uBA54\uC778\uC5D0\uC11C \uBCF5\uC18C\uC218\
  \uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
복소수는 실수에 추가적인 부분, 즉 허수 단위 'i'를 포함하여 실수를 확장합니다. 프로그래머들은 신호 처리, 전자기 이론, 프랙탈 등의 분야에서 음수의 제곱근을 포함하는 계산이 일상적으로 필요한 다양한 도메인에서 복소수를 사용합니다.

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
