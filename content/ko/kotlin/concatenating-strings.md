---
title:    "Kotlin: 문자열 연결하기"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것에 대해 생각해보신 적이 있나요? 만약 코틀린 프로그래밍에서 문자열을 합치려면 어떻게 해야 할까요? 이 블로그 포스트에서는 문자열을 연결하는 것에 대해 알아보도록 하겠습니다.

## 한글로 코딩하고 싶다면?

예를 들어, "안녕하세요"와 "여러분"을 연결하면 어떻게 될까요? 우리는 우선 문장의 형식이나 구두점 등을 고려하지 않고 단순히 두 문자열을 결합하고 싶을 것입니다. 이를 위해서는 "+" 기호를 사용하여 다음과 같이 코딩할 수 있습니다.

```Kotlin
val hello = "안녕하세요"
val everyone = "여러분"
val greeting = hello + everyone
print(greeting)

// 출력 결과: 안녕하세요여러분
```

문자열 연결을 위해 "+" 기호를 사용하는 것은 매우 간단한 방법입니다.

## 깊이 들어가보기

하지만 "+" 기호는 문자열을 결합할 때마다 새로운 문자열 객체를 생성합니다. 이는 매우 비효율적일 수 있습니다. 따라서 더 나은 방법은 문자열을 합치는 대신 문자열 템플릿을 사용하는 것입니다.

```Kotlin
val greeting = "안녕하세요" + "여러분"

// 문자열 템플릿을 사용하면 다음과 같이 작성할 수 있습니다.
val greeting = "${hello}${everyone}"
```

문자열 템플릿을 사용하면 새로운 문자열 객체를 생성하지 않고도 합칠 수 있으므로 더 효율적입니다.

## 더 많은 정보

이것 외에도 코틀린에서는 문자열을 합치기 위해 다양한 방법을 제공합니다. 예를 들어, ".concat()" 메서드를 사용하거나 StringBuilder를 사용할 수도 있습니다. 하지만 가장 간단한 방법은 "+" 기호나 문자열 템플릿을 사용하는 것입니다.

## 관련 포스트

[코틀린 공식 문서 - 문자열 템플릿](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)

[코딩스쿨 - 문자열 연결하기](https://codingschool.info/post/concatenate-strings-kotlin/)

[백준 온라인 저지 - 문자열 합치기](https://www.acmicpc.net/problem/10953)