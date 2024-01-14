---
title:    "Kotlin: 테스트 작성하기"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## 왜

프로그램을 작성할 때 테스트를 작성하는 이유는 코드를 안정적이고 신뢰할 수 있게 만들기 위해서입니다.

## 할 수 있는 방법

```Kotlin
// 기본적인 테스트 작성하기

fun sum(a: Int, b: Int): Int {
    return a + b
}

fun main(args: Array<String>) {
    val result = sum(5, 10)
    println(result)
}

```

`sum` 함수의 결과는 예상한 것과 같이 15가 되는지 확인해보세요.

```Kotlin
// 조건에 따른 테스트 작성하기

fun isEven(number: Int): Boolean {
    return number % 2 == 0
}

fun main(args: Array<String>) {
    val result = isEven(4)
    println(result)
}
```

`isEven` 함수가 짝수인지 아닌지를 정확하게 판단하는지 테스트해보세요.

## 깊게 파기

테스트를 작성하는 것은 코드를 디버깅하고 버그를 찾는 데 유용합니다. 또한 코드의 정확성을 증명하고 코드를 리팩토링하는 데 도움이 됩니다. 테스트를 작성할 때는 코드를 작성하는 것보다 일찍 시작하는 것이 좋습니다. 또한 작성한 테스트는 앞으로 코드를 변경하거나 개선할 때도 유용하게 사용할 수 있습니다.

## __See Also__
- [Kotlin Test 기초](https://kotlinlang.org/docs/tutorials/jvm-get-started.html)
- [Kotlin Test 문서](https://kotlinlang.org/api/latest/kotlin.test/index.html)
- [코드 품질 개선을 위한 테스트 작성 메소드](https://academy.realm.io/kr/posts/kau-examining-code-quality-unit-tests-android/)