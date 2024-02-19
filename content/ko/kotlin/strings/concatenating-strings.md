---
aliases:
- /ko/kotlin/concatenating-strings/
date: 2024-01-20 17:35:03.296516-07:00
description: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uC774\uB780, \uB2E8\uC5B4\uB098 \uBB38\
  \uC7A5\uC744 \uC774\uC5B4\uD558\uB098\uC758 \uBB38\uC790\uC5F4\uB85C \uB9CC\uB4DC\
  \uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\
  \uC774\uD130\uB97C \uD569\uCE58\uAC70\uB098 \uBA54\uC2DC\uC9C0\uB97C \uAD6C\uC131\
  \uD560 \uB54C \uC774 \uBC29\uBC95\uC744 \uD65C\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-18 23:09:06.137161
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uC774\uB780, \uB2E8\uC5B4\uB098 \uBB38\uC7A5\
  \uC744 \uC774\uC5B4\uD558\uB098\uC758 \uBB38\uC790\uC5F4\uB85C \uB9CC\uB4DC\uB294\
  \ \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\
  \uD130\uB97C \uD569\uCE58\uAC70\uB098 \uBA54\uC2DC\uC9C0\uB97C \uAD6C\uC131\uD560\
  \ \uB54C \uC774 \uBC29\uBC95\uC744 \uD65C\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
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
