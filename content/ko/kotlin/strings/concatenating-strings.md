---
date: 2024-01-20 17:35:03.296516-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD574\uC694?) \uBB38\uC790\uC5F4 \uC5F0\
  \uACB0\uC740 \uD504\uB85C\uADF8\uB798\uBC0D\uC758 \uCD08\uCC3D\uAE30\uBD80\uD130\
  \ \uC788\uC5C8\uC2B5\uB2C8\uB2E4. \uACFC\uAC70\uC5D4 \uBA54\uBAA8\uB9AC\uC640 \uCC98\
  \uB9AC \uB2A5\uB825\uC774 \uC81C\uD55C\uC801\uC774\uC5B4\uC11C \uD6A8\uC728\uC801\
  \uC778 \uBB38\uC790\uC5F4 \uCC98\uB9AC\uAC00 \uC911\uC694\uD588\uC2B5\uB2C8\uB2E4\
  . \uCF54\uD2C0\uB9B0\uC5D0\uC11C\uB294 + \uC5F0\uC0B0\uC790, \uBB38\uC790\uC5F4\
  \ \uD15C\uD50C\uB9BF, \uADF8\uB9AC\uACE0 `StringBuilder` \uD074\uB798\uC2A4 \uB4F1\
  \ \uB2E4\uC591\uD55C \uBC29\uBC95\uC774 \uC81C\uACF5\uB429\uB2C8\uB2E4.\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.904450-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD574\uC694?) \uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740\
  \ \uD504\uB85C\uADF8\uB798\uBC0D\uC758 \uCD08\uCC3D\uAE30\uBD80\uD130 \uC788\uC5C8\
  \uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

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
