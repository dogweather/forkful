---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:50.224399-07:00
description: "\uBC29\uBC95: Kotlin\uC5D0\uC11C\uB294 \uC678\uBD80 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uAC00 \uD544\uC694 \uC5C6\uC774 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\uC758 \uCCAB\
  \ \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C \uB9CC\uB4E4 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4. Kotlin\uC758 \uBB38\uC790\uC5F4 \uCC98\uB9AC \uBC29\uC2DD\uC740 \uC774\uB7EC\
  \uD55C \uC791\uC5C5\uC744 \uAC04\uACB0\uD558\uACE0 \uC9C1\uAD00\uC801\uC73C\uB85C\
  \ \uB9CC\uB4ED\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.147537-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\uC5D0\uC11C\uB294 \uC678\uBD80 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00\
  \ \uD544\uC694 \uC5C6\uC774 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC \uD568\uC218\
  \uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C\
  \ \uB300\uBB38\uC790\uB85C \uB9CC\uB4E4 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

## 방법:
Kotlin에서는 외부 라이브러리가 필요 없이 표준 라이브러리 함수를 사용하여 문자열의 첫 글자를 대문자로 만들 수 있습니다. Kotlin의 문자열 처리 방식은 이러한 작업을 간결하고 직관적으로 만듭니다.

### 문자열 전체를 대문자로 변환:
```kotlin
val message = "hello, world!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // 출력: HELLO, WORLD!
```

### 첫 글자만 대문자로 변환:
Kotlin 1.5부터 `capitalize()` 함수는 사용되지 않으며, 소문자인지 확인하여 대문자로 변환하는 람다와 함께 `replaceFirstChar`의 조합으로 대체되었습니다.

```kotlin
val greeting = "hello, world!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // 출력: Hello, world!
```

이 접근 방식은 문장의 나머지 부분을 원래 형태로 유지하면서 첫 글자만 대문자로 변경합니다.
