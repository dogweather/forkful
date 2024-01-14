---
title:                "Kotlin: 문자열 연결하기"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜
문자열을 연결하는 것에 참여하는 이유는 우리가 문자열을 조작 할 때 필요합니다. 예를 들어, 다른 문자열에서 일부 단어를 동적으로 조합하거나 자주 사용되는 문구를 만들어야하는 경우가 있습니다.

## 방법
```Kotlin
// 새로운 변수 createdString을 만들고 세 개의 문자열을 연결합니다.
val string1 = "안녕하세요, "
val string2 = "저는 "
val string3 = "코틀린을 배우고 있습니다."
val createdString = string1 + string2 + string3

// 출력: 안녕하세요, 저는 코틀린을 배우고 있습니다.
print(createdString)

// 변수와 문자열을 결합하여 출력할 수도 있습니다.
val name = "홍길동"
val greeting = "안녕하세요, " + name + "님."
print(greeting)

// 출력: 안녕하세요, 홍길동님.
```

## 딥 다이브
문자열 연결에 대해 더 알아보겠습니다. Kotlin에서는 "+" 연산자를 사용하여 두 개 이상의 문자열을 연결 할 수 있습니다. 또한 문자열 템플릿을 사용하여 변수와 문자열을 조합 할 수도 있습니다. 예를 들어, 위의 예제에서 "greeting" 변수를 다음과 같이 수정할 수 있습니다.

```Kotlin
val name = "홍길동"
val greeting = "안녕하세요, ${name}님."
```

이렇게하면 변수 "name"의 값이 동적으로 포함되어 출력되는 문자열이 생성됩니다.

## 참조
**더 많은 정보를 원하신다면, 아래 링크들을 참조해보세요.**

- [Kotlin 문자열 연산자](https://kotlinlang.org/docs/strings.html#string-concatenation)
- [코틀린 문자열 템플릿](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
- [Learn X in Y minutes에서 코틀린 배우기](https://learnxinyminutes.com/docs/kotlin/)