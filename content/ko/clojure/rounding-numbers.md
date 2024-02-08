---
title:                "숫자 반올림하기"
aliases:
- ko/clojure/rounding-numbers.md
date:                  2024-01-26T03:43:48.992190-07:00
model:                 gpt-4-0125-preview
simple_title:         "숫자 반올림하기"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/rounding-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
숫자 반올림은 숫자를 가장 가까운 정수로, 또는 특정 소수 정밀도까지 조정하는 것입니다. 우리는 숫자를 반올림하여 인간의 가독성을 단순화하고, 계산 부하를 줄이며, 특정 수치 요구 사항을 충족하기 위해 반올림합니다.

## 어떻게:
Clojure에서 주로 `Math/round`, `Math/floor`, `Math/ceil`을 사용합니다:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

특정 소수점 자리수에 대해서는 곱하고, 반올림하고, 나눕니다:

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## 심층 분석
팬시한 프로그래밍 언어가 등장하기 전에는 반올림이 수동 과정이었습니다. 예를 들어, 주판이나 종이를 사용했습니다. 프로그래밍에서 반올림은 부동 소수점의 정밀도 제한 때문에 숫자 표현에 있어 중요합니다.

반올림을 위한 대안으로는 정밀도 제어를 위한 `BigDecimal` 클래스 사용이나 `clojure.math.numeric-tower`와 같은 고급 수학 함수용 라이브러리가 있습니다. Clojure의 `Math/round`는 Java의 `Math.round`, `Math/floor`, `Math/ceil` 함수에 의존하는데, 이는 float 및 double의 미묘한 차이를 상속받음을 의미합니다.

구현 측면에서, Clojure에서 반올림할 때는 소수점을 다룰 때 자동으로 더블 정밀도를 사용한다는 점을 기억하세요. 반올림 오류에 주의하세요!

## 참고
- Clojure 수학 API: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- Java 수학 API: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- 부동 소수점 정밀도 이해하기: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
