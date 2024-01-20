---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇이고 왜합니까?

랜덤 숫자 생성은 예측 불가능한 숫자들을 만들어내는 것입니다. 프로그래머들은 이를 사용해 게임 메카닉, 통계 분석, 보안 암호화 등에 많이 활용합니다.

## 이렇게 해보세요:

아래 코드는 0에서 100까지 범위에서 랜덤 정수를 생성합니다.

```Kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(101)
    println(randomNumber)
}
```
실행 결과는 매번 달라질 것입니다. 예를 들어, `42` 혹은 `85`등이 출력될 수 있습니다.

## 깊은 탐구:

랜덤 숫자 생성은 계산학의 초창기부터 다루어진 주제입니다. `Math.random()` 같은 기존 방법들이 있지만, Kotlin 에서는 `Random` 클래스를 사용하는 것이 더 권장됩니다. 이는 더 넓은 기능 범위와, 일관성 있게 랜덤값을 생성하기 때문입니다. 

아래는 다른 랜덤 숫자 혹은 범위를 생성하는 예제들입니다:

```Kotlin
// 랜덤 실수 생성
val randomDouble = Random.nextDouble()
println(randomDouble)

// 주어진 범위 내에서 랜덤 정수 생성
val randomWithinRange = Random.nextInt(5, 11)
println(randomWithinRange)
```
또한, `Random` 클래스에서 제공하는 여러 함수들을 활용하면, 배열이나 리스트에서 랜덤한 요소를 선택하는 것도 가능합니다.

```Kotlin
// 리스트에서 랜덤 요소 선택
val myList = listOf("apple", "banana", "cherry")
val randomElement = myList.random()
println(randomElement)
```

## 참고자료:

Kotlin 공식문서의 Random 클래스 설명: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
이외에도, 더 깊게 알고 싶다면 다음 링크를 참고해보세요:
- [https://en.wikipedia.org/wiki/Random_number_generation](https://en.wikipedia.org/wiki/Random_number_generation)
- [https://developer.android.com/reference/kotlin/kotlin.random.Random](https://developer.android.com/reference/kotlin/kotlin.random.Random)