---
date: 2024-01-27 20:35:03.380419-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB09C\uC218 \uC0DD\uC131\
  \uC740 \uC608\uCE21 \uAC00\uB2A5\uD55C \uD328\uD134\uC774 \uC5C6\uB294 \uC22B\uC790\
  \uB97C \uB9CC\uB4DC\uB294 \uAC83\uC5D0 \uB300\uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC2DC\uBBAC\uB808\uC774\uC158, \uC54C\uACE0\
  \uB9AC\uC998 \uD14C\uC2A4\uD2B8, \uAC8C\uC784, \uADF8\uB9AC\uACE0 \uBCF4\uC548 \uC751\
  \uC6A9 \uD504\uB85C\uADF8\uB7A8 \uB4F1 \uB2E4\uC591\uD55C \uC774\uC720\uB85C \uC774\
  \ \uC791\uC5C5\uC744 \uC218\uD589\uD558\uB294\uB370, \uC5EC\uAE30\uC11C \uBD88\uD655\
  \uC2E4\uC131\uC740 \uD604\uC2E4\uC801\uC774\uAC70\uB098 \uC548\uC804\uD55C \uACB0\
  \uACFC\uB97C \uB2EC\uC131\uD558\uB294 \uB370 \uD575\uC2EC\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.165625-06:00'
model: gpt-4-0125-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB09C\uC218 \uC0DD\uC131\uC740\
  \ \uC608\uCE21 \uAC00\uB2A5\uD55C \uD328\uD134\uC774 \uC5C6\uB294 \uC22B\uC790\uB97C\
  \ \uB9CC\uB4DC\uB294 \uAC83\uC5D0 \uB300\uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC2DC\uBBAC\uB808\uC774\uC158, \uC54C\uACE0\uB9AC\
  \uC998 \uD14C\uC2A4\uD2B8, \uAC8C\uC784, \uADF8\uB9AC\uACE0 \uBCF4\uC548 \uC751\uC6A9\
  \ \uD504\uB85C\uADF8\uB7A8 \uB4F1 \uB2E4\uC591\uD55C \uC774\uC720\uB85C \uC774 \uC791\
  \uC5C5\uC744 \uC218\uD589\uD558\uB294\uB370, \uC5EC\uAE30\uC11C \uBD88\uD655\uC2E4\
  \uC131\uC740 \uD604\uC2E4\uC801\uC774\uAC70\uB098 \uC548\uC804\uD55C \uACB0\uACFC\
  \uB97C \uB2EC\uC131\uD558\uB294 \uB370 \uD575\uC2EC\uC785\uB2C8\uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
---

{{< edit_this_page >}}

## 무엇 & 왜?

프로그래밍에서 난수 생성은 예측 가능한 패턴이 없는 숫자를 만드는 것에 대한 것입니다. 프로그래머들은 시뮬레이션, 알고리즘 테스트, 게임, 그리고 보안 응용 프로그램 등 다양한 이유로 이 작업을 수행하는데, 여기서 불확실성은 현실적이거나 안전한 결과를 달성하는 데 핵심입니다.

## 방법:

Kotlin은 표준 라이브러리를 통해 난수를 생성하는 간단한 방법을 제공합니다. 다음은 서로 다른 유형의 난수 값을 생성하는 방법입니다:

### 정수형 난수 생성

특정 범위 내에서 정수형 난수를 생성하려면:

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100) // 1과 99 사이의 난수를 생성합니다
    println(randomNumber)
}
```

### 더블형 난수 생성

마찬가지로, 더블형 난수를 생성합니다:

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // 1.0과 10.0 사이의 난수를 생성합니다
    println(randomDouble)
}
```

### 불리언형 난수 생성

불리언형 난수 값을 생성하려면:

```kotlin
import kotlin.random.Random

fun main() {
    val randomBoolean = Random.nextBoolean() // 무작위로 true 또는 false를 생성합니다
    println(randomBoolean)
}
```

### 재현 가능한 결과를 위한 시드 설정

테스트와 같은, 재현 가능한 난수 시퀀스가 필요한 경우에는 난수 생성기에 시드를 설정할 수 있습니다:

```kotlin
import kotlin.random.Random

fun main() {
    val seed = 12345L
    val random = Random(seed)
    val randomNumber = random.nextInt(1, 100)
    println(randomNumber)
}
```

## 심층 탐구

Kotlin 표준 라이브러리의 난수 생성 접근 방식은 내부적으로 Java의 `java.util.Random`을 활용하며, 사용의 용이성과 성능의 균형을 보장합니다. 하지만, 이러한 방법들이 생성하는 난수는 결정론적 과정을 사용하여 생성되기 때문에 난수처럼 보이는 유사난수임을 주의할 필요가 있습니다.

대부분의 응용 프로그램에서 Kotlin의 `Random` 클래스로 제공되는 무작위성은 충분합니다. 하지만, 무작위성의 품질이 매우 중요한 보안 관련 응용 프로그램, 예를 들어 암호화에서는 성능의 손실 가능성이 있더라도 더 높은 품질의 난수를 제공하는 `java.security.SecureRandom`을 고려해야 할 수 있습니다. SecureRandom은 암호화 작업을 위해 특별히 설계되었으며, 더 높은 품질의 무작위성을 제공합니다.

Kotlin은 Java의 난수 생성 메커니즘 위에 Kotlin 친화적인 API를 제공하여, Kotlin 프로젝트 내에서 더 관용적이고 간결하게 사용할 수 있도록 합니다. 항상 그렇듯이, 무작위성을 다룰 때 프로그래머들은 사용 사례를 신중하게 고려하여 작업에 가장 적합한 도구를 선택해야 합니다.
