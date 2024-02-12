---
title:                "문자열 연결하기"
date:                  2024-01-20T17:35:03.296516-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 연결하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하죠?)
문자열 연결이란, 단어나 문장을 이어하나의 문자열로 만드는 것입니다. 프로그래머들은 데이터를 합치거나 메시지를 구성할 때 이 방법을 활용합니다.

## How to: (어떻게 해요?)
```kotlin
fun main() {
    val greeting = "안녕"
    val name = "세상"
    val exclamation = "!"

    // 방법 1: 더하기 연산자 사용
    val message1 = greeting + " " + name + exclamation
    println(message1) // 출력: 안녕 세상!

    // 방법 2: string templates 사용
    val message2 = "$greeting $name$exclamation"
    println(message2) // 출력: 안녕 세상!

    // 방법 3: StringBuilder 사용
    val message3 = StringBuilder()
        .append(greeting)
        .append(" ")
        .append(name)
        .append(exclamation)
        .toString()
    println(message3) // 출력: 안녕 세상!
}
```

## Deep Dive (깊은 고민)
문자열 연결은 프로그래밍의 초창기부터 있었습니다. 과거엔 메모리와 처리 능력이 제한적이어서 효율적인 문자열 처리가 중요했습니다. 코틀린에서는 + 연산자, 문자열 템플릿, 그리고 `StringBuilder` 클래스 등 다양한 방법이 제공됩니다.

+ 연산자는 가독성이 좋지만, 더 많은 객체를 생성할 수 있어 메모리 사용이 비효율적일 수 있습니다. 문자열 템플릿은 변수를 직접 문자열 안에 넣어 코드를 간결하게 만들어 줍니다. 큰 데이터를 다룰 때는 `StringBuilder`가 유용합니다. 이것은 내부 버퍼에 문자열을 추가하는 방식으로, 굳이 새로운 문자열 객체를 만들지 않아 메모리 관리에 효과적입니다.

## See Also (추가 정보)
- Kotlin 공식 문서에서 문자열 연결하기: [Kotlin Docs - Strings](https://kotlinlang.org/docs/basic-types.html#strings)
- `StringBuilder` 사용법 참고하기: [Kotlin Docs - StringBuilder](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)