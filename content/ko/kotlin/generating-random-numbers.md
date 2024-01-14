---
title:    "Kotlin: 랜덤 숫자 생성하기."
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜
랜덤한 숫자를 생성하는 것이 유용한 이유는 다양합니다. 예를 들어, 게임이나 암호화와 같은 분야에서는 랜덤한 숫자를 사용하여 재미있는 경험이나 보안성을 향상시킬 수 있습니다.

## 어떻게
Kotlin에서는 `Random` 클래스를 사용하여 랜덤한 숫자를 생성할 수 있습니다. 아래는 `nextInt()` 메서드를 사용하여 1부터 10까지의 랜덤한 숫자를 생성하는 예제입니다.

```Kotlin
val random = Random() // Random 클래스의 인스턴스 생성
val randomNumber = random.nextInt(10) + 1 // 1부터 10까지의 랜덤한 숫자 생성
println(randomNumber) // 예시 출력: 8
```

`nextInt()` 메서드는 매개변수로 전달된 숫자 미만의 범위 내에서 랜덤한 숫자를 생성합니다. 따라서 위 예제에서는 9까지의 숫자가 생성될 수 있고, +1을 함으로써 10까지의 숫자가 생성되도록 조정합니다.

## 깊이 파헤치기
랜덤한 숫자를 생성하는 알고리즘은 매우 복잡합니다. 일반적으로 컴퓨터는 사람과 달리 완전히 무작위한 숫자를 생성하지 않고, 사람이 눈으로는 볼 수 없는 시드(seed)라는 값에 기반하여 숫자를 생성합니다. 시드는 사용된 알고리즘에 따라 다르며 랜덤한 숫자의 시퀀스를 결정합니다.

Kotlin에서 사용되는 `Random` 클래스는 Mersenne Twister 알고리즘을 사용합니다. 이 알고리즘은 시드의 주기가 적어서 오랜 시간 동안 랜덤한 숫자를 생성할 수 있습니다. 또한, 이 알고리즘은 많은 수학적 계산을 필요로 하기 때문에 현실적으로는 완전히 랜덤한 숫자를 생성하지는 않습니다.

## 참고자료
- [Kotlin 공식 문서](https://kotlinlang.org/docs/reference/basic-types.html#floating-point-types)
- [Java에서 난수 생성하기](https://www.geeksforgeeks.org/generate-random-numbers-in-java/)

# 참고 자료
- [Kotlin 공식 문서](https://kotlinlang.org/docs/reference/basic-types.html#floating-point-types)
- [Java에서 난수 생성하기](https://www.geeksforgeeks.org/generate-random-numbers-in-java/)