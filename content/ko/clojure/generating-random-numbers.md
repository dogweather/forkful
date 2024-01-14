---
title:    "Clojure: 난수 생성하기"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜?

랜덤한 숫자를 생성하는 일을 왜 할까요? 우리가 많은 프로그래밍 작업을 수행할 때 필요한 것들 중 하나는 랜덤한 값을 가진 데이터입니다. 예를 들어서 게임을 만들거나, 사용자에게 랜덤한 문제를 제공하거나, 무작위로 데이터를 처리하거나 할 때 랜덤한 숫자 생성 기능이 필요합니다. 또는 스터디나 시험 때 학생들에게 랜덤한 순서로 문제를 내주는 것도 가능하죠. 여러분이 할 수 있을 거라고 생각하지 못했던 새로운 아이디어를 찾을 수도 있습니다. 하지만 이러한 모든 작업에는 랜덤한 숫자 생성이 필수적이라는 것을 기억해 주세요.

## 어떻게 할까요?

우선 우리는 랜덤한 숫자를 생성하는 데 필요한 함수를 알아야 합니다. Clojure에서는 `rand` 함수를 사용하면 됩니다. 이 함수는 0과 1 사이의 랜덤한 숫자를 생성합니다. 예제를 먼저 살펴보겠습니다.

```Clojure
(rand)
```

위의 코드를 실행하면 실행할 때마다 다른 랜덤한 숫자가 생성됩니다.

```Clojure
(rand) ; 0.5424162125660655
(rand) ; 0.10781328403267403
```

우리는 `rand` 함수를 사용하여 원하는 범위의 랜덤한 숫자를 생성할 수도 있습니다. 예를 들어서 1부터 10 사이의 숫자를 생성하려면 다음과 같이 할 수 있습니다.

```Clojure
(+ (rand-int 10) 1)
```

위의 코드는 1부터 10 사이의 랜덤한 정수를 생성합니다. 이를 실행하면 다음과 같은 결과가 나오게 됩니다.

```Clojure
(+ (rand-int 10) 1) ; 9
(+ (rand-int 10) 1) ; 7
```

## 깊이 나가보기

Clojure에서 랜덤한 숫자를 생성하는 `rand` 함수는 기본적으로 Java의 `java.util.Random` 클래스를 사용합니다. 이 클래스는 구현된 알고리즘에 따라 랜덤한 값을 생성하기 때문에 종종 보안적인 문제로 이어질 수 있습니다. 또한 무작위성이 필요한 경우, 예를 들어서 노래를 재생할 때나 게임을 할 때에는 `SecureRandom` 클래스를 사용하는 것이 좋습니다.

또한 Clojure는 랜덤한 숫자를 생성하기 위해 `Math.random` 함수를 사용할 수도 있습니다. 이 함수는 더 빠른 속도로 랜덤한 값을 생성하지만 더 높은 수준의 무작위성을 보장하지는 않습니다.

## 더 알아보기

아래의 링크를 통해 더 많은 정보를 얻을 수 있습니다.

https://clojure.org/reference/java_interop#_java_util_random

https://docs.oracle.com/javase/8/docs/api/java/util/Random.html

# 더 알아보기

아래의 링크를 통해 더 많은 정보를 얻을 수 있습니다.

https://clojure.org/reference/java_interop#_java_util_random

https://docs.oracle.com/javase/8/docs/api/java/util/Random.html