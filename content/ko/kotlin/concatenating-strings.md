---
title:                "문자열 연결하기"
html_title:           "Kotlin: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜
이번 글에서는 Kotlin에서 문자열을 연결하는 것에 대해 알아보겠습니다. 문자열 연결은 우리가 프로그래밍을 할 때 빈번하게 사용되는 기능이며, 이를 알고 있으면 더 효율적인 코드를 작성할 수 있습니다.

## 방법
먼저, 우리는 ```+``` 연산자를 이용하여 두 문자열을 연결할 수 있습니다. 예를 들어, 아래의 코드는 "Hello"와 "Kotlin"이라는 두 문자열을 연결하여 "Hello Kotlin"이라는 하나의 문자열을 생성합니다.
```Kotlin
val str1 = "Hello"
val str2 = "Kotlin"
val result = str1 + str2
println(result) // 출력 결과: Hello Kotlin
```
또는 우리는 ```StringBuilder``` 클래스를 사용하여 여러 개의 문자열을 연결할 수도 있습니다. 이 클래스는 불변성(immutable)의 문제를 해결하기 위해, 새로운 문자열을 생성하는 대신 기존의 문자열에 문자를 계속 추가하는 방식으로 작동합니다. 아래의 예시 코드를 보면 더욱 명확해집니다.
```Kotlin
val str1 = "Hello"
val str2 = "Kotlin"
val sb = StringBuilder()
sb.append(str1)
sb.append(str2)
println(sb.toString()) // 출력 결과: Hello Kotlin
```

## 깊이 파고들기
Kotlin에서 문자열 연결은 또 다른 흥미로운 점이 있습니다. 이미 ```String``` 클래스는 불변성을 가지기 때문에, 새로운 문자열이 생성될 때마다 새로운 인스턴스가 할당됩니다. 이는 성능 측면에서 비효율적일 수 있습니다. 하지만 ```StringBuilder```는 문자열을 수정하면서도 동일한 인스턴스를 사용하기 때문에 성능이 향상됩니다. 이를 통해 문자열 연결을 할 때 메모리와 성능 면에서 효율적인 코드를 작성할 수 있습니다.

## 살펴보기
Kotlin 공식 문서에서 문자열 연결에 대한 더 많은 정보를 얻을 수 있습니다.
- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [StringBuilder Class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)