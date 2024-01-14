---
title:                "Clojure: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜

랜덤 숫자를 생성하는 것에 대해 관심을 가질 이유는 다양합니다. 일반적으로 프로그래밍에서 랜덤 숫자를 사용하는 경우는 데이터를 무작위로 분석하거나, 게임에서 새로운 요소를 생성하는 등에 활용하기 때문입니다.

# 어떻게

랜덤 숫자를 생성하는 방법에는 다양한 방법이 있지만, Clojure에서 가장 흔한 방법은 ```rand``` 함수를 사용하는 것입니다. 이 함수는 기본적으로 0과 1 사이의 랜덤한 부동소수점 숫자를 생성합니다.

```Clojure
(rand) ;; 0.453976359654876
```

만약 원하는 범위를 지정하고 싶다면, ```rand``` 함수에 인자를 추가해주면 됩니다.

```Clojure
(rand 10) ;; 6.908134537584917
```

이렇게 하면 0에서 10 사이의 랜덤한 숫자가 생성됩니다. 또한, 반복적으로 랜덤 숫자를 생성하고 싶을 때는 ```repeatedly``` 함수를 사용할 수 있습니다.

```clojure
(repeatedly 5 rand) ;; (0.78291393967237 0.31426788080114737 0.656375878183015 0.15081684175763522 0.6871242286722354)
```

이 방법으로 5개의 랜덤 숫자를 생성할 수 있습니다. 만약 정수형 숫자가 필요하다면, ```rand-int``` 함수를 사용하면 됩니다.

```Clojure
(rand-int 10) ;; 8
```

이렇게 하면 0에서 10 사이의 정수형 랜덤 숫자가 생성됩니다.

# 심층 분석

Clojure에서 랜덤 숫자를 생성하는 함수들은 사실 의사난수(Pseudorandom Number)를 생성합니다. 이는 시드값(seed value)을 이용해서 순서대로 나열하는 숫자로, 완전한 무작위 숫자는 아닙니다. 하지만 일반적인 용도에서는 큰 문제가 되지 않으며, 필요하다면 직접 시드값을 지정해서 더 복잡한 의사난수를 생성할 수도 있습니다.

# 관련 자료

- [Clojure 공식 문서: 랜덤 숫자 생성](https://clojuredocs.org/clojure.core/rand)
- [Clojure 빌트인 함수: rand vs. rand-int 속도 비교](https://github.com/functional-koans/clojure-koans/wiki/Clojure-built-in,-rand-vs.-rand-int-comparison)
- [Clojure 랜덤 숫자 생성 과정 분석](https://medium.com/@hkaid/random-number-generation-in-clojure-and-how-a-psuedorandom-number-generator-works-82f7e7a2e5f1)