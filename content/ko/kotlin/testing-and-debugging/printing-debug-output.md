---
date: 2024-01-20 17:53:13.327531-07:00
description: "How to: (\uBC29\uBC95) Kotlin\uC5D0\uC11C\uB294 `println()` \uD568\uC218\
  \uB85C \uB514\uBC84\uADF8 \uBA54\uC2DC\uC9C0\uB97C \uC27D\uAC8C \uCD9C\uB825\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.916249-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) Kotlin\uC5D0\uC11C\uB294 `println()` \uD568\uC218\uB85C \uB514\
  \uBC84\uADF8 \uBA54\uC2DC\uC9C0\uB97C \uC27D\uAC8C \uCD9C\uB825\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
title: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uCC0D\uC5B4\uBCF4\uAE30"
weight: 33
---

## How to: (방법)
Kotlin에서는 `println()` 함수로 디버그 메시지를 쉽게 출력할 수 있습니다.

```Kotlin
fun main() {
    var temperature = 21
    println("Current temperature: $temperature")

    temperature = 23
    println("New temperature: $temperature")
}
```
출력:
```
Current temperature: 21
New temperature: 23
```

`println()`는 객체의 `toString()` 메서드를 호출하여 문자열을 출력하니 복잡한 객체도 간단히 찍어볼 수 있습니다.

```Kotlin
data class Weather(val temperature: Int, val humidity: Double)

fun main() {
    val currentWeather = Weather(temperature = 25, humidity = 0.65)
    println(currentWeather)
}
```
출력:
```
Weather(temperature=25, humidity=0.65)
```

## Deep Dive (심화 탐구)
디버그 출력은 과거 터미널 또는 명령줄 인터페이스에서 프로그램의 상태를 모니터링하기 위해 개발자들이 사용한 오래된 방법입니다. 요즘에는 IDE나 디버거를 사용해 더 강력한 디버깅을 할 수 있지만, `println()`은 여전히 그 간편함으로 인해 자주 사용됩니다.

대안으로는 `log` 라이브러리를 사용하는 방법이 있습니다. 이는 로그 레벨에 따라 출력을 관리하고, 필요할 때만 특정 종류의 메시지를 출력하도록 도와줍니다.

구현 세부사항으로는 `println()`이 표준 출력 스트림인 `System.out`으로 메시지를 보내고, 이로 인해 퍼포먼스 이슈가 발생할 수 있다는 점입니다. 프로덕션 코드에서 너무 많은 디버그 출력은 오히려 문제를 야기할 수 있습니다.

## See Also (참고 자료)
- [로깅을 위한 SLF4J 사용법](https://www.slf4j.org/manual.html)
- JetBrains의 [IntelliJ IDEA 디버거](https://www.jetbrains.com/help/idea/debugging-code.html)
