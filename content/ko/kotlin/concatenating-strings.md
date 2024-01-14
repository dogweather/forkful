---
title:                "Kotlin: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## 이유
스트링을 연결하는 것에 대해 관심이 있는 사람들이 읽는 블로그입니다.

## 방법
스트링을 연결하는 간단한 예제를 아래 코드 블록에서 볼 수 있습니다.

```Kotlin
val str1 = "Hello"
val str2 = "world"
val result = str1 + " " + str2
println(result) // Output: Hello world
```

위 예제에서 우리는 `+` 연산자를 사용하여 두 개의 스트링을 연결할 수 있습니다. 또한, 코틀린에서는 문자열 보간(string interpolation)을 사용하여 더 간결하게 스트링을 연결할 수 있습니다.

```Kotlin
val name = "Jane"
val age = 25
val result = "My name is ${name} and I am ${age} years old."
println(result) // Output: My name is Jane and I am 25 years old.
```

## 깊게 들어가기
스트링 연결은 프로그래밍에서 매우 일반적으로 사용되는 작업입니다. 그 이유는 스트링이나 다른 타입의 값을 출력할 때 읽기 쉽고 이해하기 쉬운 결과를 만들어내기 때문입니다. 또한, 스트링 연결은 문자열 템플릿이나 형식 문자열을 생성하는 데에도 사용될 수 있습니다.

코틀린에서는 `StringBuilder` 클래스를 사용하여 더 빠르고 효율적으로 스트링을 연결할 수 있습니다. 이 클래스는 가변성을 제공하며, 여러 개의 스트링을 하나의 스트링으로 합칠 때 효율적으로 작동합니다.

## 또 보기
- [Kotlin 문자열 연결 가이드](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [코틀린에서 문자열 보간 사용하기](https://kotlinlang.org/docs/reference/basic-types.html#string-interpolation)
- [StringBuilder 클래스에 대한 자세한 정보](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/index.html)