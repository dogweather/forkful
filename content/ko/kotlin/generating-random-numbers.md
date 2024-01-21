---
title:                "난수 생성하기"
date:                  2024-01-20T17:49:58.168395-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇을 위해? 왜?)
랜덤 숫자를 생성하는 것은 말 그대로 예측할 수 없는 숫자를 만드는 프로세스입니다. 프로그래머들은 게임, 시뮬레이션, 보안 시스템에서 불확실성을 제공하거나 테스트 데이터를 생성하기 위해 이를 사용합니다.

## How to: (방법)
Kotlin에서 랜덤 숫자를 생성하는 방법을 보여드립니다.

```Kotlin
import kotlin.random.Random

fun main() {
    val randomValue = Random.nextInt(0, 100) // 0부터 99까지의 랜덤 값을 생성
    println(randomValue)
}
```
예상 출력물: `42` (실제 출력값은 실행 때마다 달라집니다)

```Kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // 1.0부터 9.999...까지의 랜덤 실수 값을 생성
    println(randomDouble)
}
```
예상 출력물: `5.789...` (실제 출력값은 실행 때마다 달라집니다)

## Deep Dive (심도 있는 탐구)
처음에는 단순히 `Math.random()`을 사용해 랜덤 값을 얻었습니다. Kotlin은 `kotlin.random.Random`을 통한 더욱 풍부한 API를 제공하며, 다양한 타입이나 범위의 랜덤 값을 생성할 수 있습니다. 실제로, 컴퓨터는 진정한 의미의 랜덤 값을 생성할 수 없으며, '의사 난수 생성기(Pseudo-Random Number Generators, PRNGs)'를 사용합니다. 이들은 시작 점(seed)를 기반으로 일련의 숫자를 생성하는 알고리즘을 사용하여, 외견상 랜덤으로 보이는 수열을 만듭니다.

코틀린에서는 내장된 PRNG 기능을 사용하여 간결하지만 강력한 코드로 랜덤 값을 생성할 수 있습니다. `kotlin.random` 패키지는 또한 섞임(shuffling), 샘플링(sampling) 같은 일련의 확장 기능을 제공합니다.

`Random.nextInt()`나 `Random.nextDouble()` 같은 함수는 현대 암호학에 사용하기에는 충분히 안전하지 않을 수 있습니다. 보안상 민감한 작업을 위해선 `java.security.SecureRandom` 클래스를 사용하는 것이 좋습니다.

## See Also (관련 자료)
- [Kotlin 공식 문서: Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)
- [Oracle JavaDocs: SecureRandom](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
- [Wikipedia: Pseudo-Random Number Generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)