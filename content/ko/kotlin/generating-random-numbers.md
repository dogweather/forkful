---
title:                "랜덤 숫자 생성하기"
html_title:           "Kotlin: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

 왜 누군가 랜덤한 숫자를 생성하는 것에 참여할까요? 랜덤 숫자는 게임, 시뮬레이션, 보안 등 다양한 분야에서 사용될 수 있는 중요한 요소입니다. 하지만 컴퓨터에서 완벽한 랜덤 숫자를 만드는 것은 불가능하므로, 우리는 그 대신에 난수 생성기를 사용합니다.

## 방법

랜덤 숫자를 생성하는 방법은 Kotlin에서 매우 간단합니다. 아래에 제시된 예시 코드를 통해 쉽게 이해할 수 있습니다.

```Kotlin
// 1부터 10까지의 랜덤 정수 출력
val randomNumber = (1..10).random()
println("Random Number: $randomNumber")

// 0부터 1까지의 랜덤 소수 출력
val randomDouble = Math.random()
println("Random Double: $randomDouble")

// 지정된 범위 내의 랜덤 수 출력
val randomInRange = (100..500).random()
println("Random Number in Range: $randomInRange")
```

위의 예시 코드에서 `random()` 함수를 사용하면 지정된 범위 내에서 랜덤한 숫자를 생성할 수 있습니다. 실제로 실행해보면 매번 다른 숫자가 출력되는 것을 확인할 수 있습니다.

## 깊게 들어가기

랜덤 숫자를 생성하기 위해서는 알고리즘과 시드(seed)라는 개념을 이해하는 것이 중요합니다. 알고리즘은 랜덤 숫자를 생성하는 방식을 말하고, 시드(seed)는 랜덤 숫자를 만들어내는 기반 값입니다. 보통 시드는 현재 시간이나 프로그램이 실행되는 시점의 시스템 값 등으로 설정합니다. Kotlin에서는 `random()` 함수 내에 `Random()` 생성자를 사용하여 알고리즘을 지정할 수 있습니다. 또한 `nextInt()` 함수를 사용하여 알고리즘의 범위를 지정할 수도 있습니다.

## 연관 정보

* [Java와 Kotlin에서 Random Number 생성하는 방법](https://www.baeldung.com/java-generating-random-numbers)
* [Kotlin의 임의 부동 소수점 생성기(Documentation)](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/index.html)