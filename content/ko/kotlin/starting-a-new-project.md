---
title:                "Kotlin: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜

왜 누군가 새로운 프로젝트를 시작하게 되는지 궁금하신가요? 개발자들은 새로운 기술을 배우고 싶거나, 새로운 아이디어를 구현하고 싶거나, 혹은 자신만의 프로젝트를 만들어보고 싶어서 새로운 프로젝트를 시작합니다. 또한, 새로운 프로젝트를 시작해보는 것으로 인해 더 많은 경험과 도전을 할 수 있고, 자신의 능력을 향상시킬 수 있습니다.

## 하나

새로운 프로젝트를 시작하는 방법은 어떤 것이 있을까요? 이제 Kotlin으로 프로그래밍을 시작해보겠습니다! 먼저 Kotlin의 기본 문법을 배우기 위해 간단한 예제 프로그램을 작성해보겠습니다. 아래의 코드를 따라서 입력하고 실행해보세요.

```Kotlin
fun main() {
    println("Hello World!")
}
```

위의 코드는 화면에 "Hello World!"를 출력하는 간단한 프로그램입니다. 이제 위의 코드를 분석해보겠습니다. 첫째 줄에 있는 `fun main()`은 Kotlin에서의 함수를 선언하는 방식입니다. `main`은 이 함수의 이름으로, 프로그램이 시작되는 곳입니다. 소괄호 안에는 이 함수가 받을 인자가 없으므로 비어있습니다. 둘째 줄에 있는 `println()`은 화면에 내용을 출력하는 함수입니다. 소괄호 안에 있는 "Hello World!"는 출력할 내용입니다. 따라서 위의 코드는 함수를 호출하는 방식으로 실행되기 때문에 아무런 출력이 없이 프로그램이 종료됩니다.

이제 더 복잡한 예제를 살펴보겠습니다. 아래의 코드를 따라서 입력하고 실행해보세요.

```Kotlin
fun main() {
    val name = "John"
    val age = 25
    println("My name is $name and I am $age years old.")
}
```

위의 코드는 "My name is John and I am 25 years old."를 출력하는 간단한 프로그램입니다. 이제 위의 코드를 분석해보겠습니다. 첫째 줄과 둘째 줄에서 `val` 키워드를 사용하여 변수를 선언하고 있습니다. `val`은 값을 바꿀 수 없는 변수를 선언할 때 사용합니다. 세번째 줄에서는 `println()` 함수가 호출될 때 문자열 내부에 `$` 기호를 이용하여 변수 값을 출력하도록 설정하고 있습니다. 따라서 위의 코드는 `name` 변수에 저장된 값인 "John"과 `age` 변수에 저장된 값인 25가 문자열 내부에 각각 대입되어 출력되기 때문에 "My name is John and I am 25 years old."가 출력됩니다.

다른 예제를 살펴보겠습니다. 아래의 코드를 따라서 입력하고 실행해보세요.

```Kotlin
fun main() {
    var number = 5
    println("Number is $number.")
    number = 2
    println("Now, number is $number.")
}
```

위의 코드는 "Number is 5."와 "Now, number is 2."를 차례대로 출력하는 간단한 프로그램입니다. 이제 위의 코드를 분석해보겠습니다. 첫째 줄에서 `var` 키워드를 사용하여 변수를 선언하고 있습니다. `var`은 값을 바꿀 수 있는 변수를 선언할 때 사용합니다. 둘째 줄에서는 `println()` 함수가 호출