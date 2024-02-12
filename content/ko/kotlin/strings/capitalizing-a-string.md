---
title:                "문자열 대문자화"
aliases:
- /ko/kotlin/capitalizing-a-string.md
date:                  2024-02-03T19:05:50.224399-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜?

프로그래밍에서 문자열의 첫 글자를 대문자로 변환하는 것을 말합니다. 이것은 사용자 입력을 형식화하거나 사용자 인터페이스에 텍스트를 더 표준화되거나 사용자 친화적인 방식으로 표시하는 데 유용합니다. 프로그래머들은 이 작업을 수행하여 데이터 일관성을 보장하거나 소프트웨어 애플리케이션 내에서 특정 형식 요구 사항을 충족합니다.

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
