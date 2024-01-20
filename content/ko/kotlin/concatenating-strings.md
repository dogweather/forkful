---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 그래야하는가?

문자열 연결(string concatenation)은 두 개 이상의 문자열을 한 줄로 결합하는 과정입니다. 프로그래머들은 데이터를 모니터링하고, 로깅하고, 사용자에게 메시지를 전달할 때 종종 이를 사용합니다.

## 어떻게 사용하나요?

Kotlin에서 문자열을 연결하는 방법은 수많은 편리한 방법이 있습니다. 여기 2가지 방법을 보여드리겠습니다.

첫 번째 방법은 `+` 연산자를 사용하는 것입니다.

```Kotlin
val a = "Hello, "
val b = "World!"
println(a + b) // "Hello, World!"
```
이제 두 번째 방법은 문자열 템플릿을 사용하는 것입니다. 이 방법은 문자열 사이에 변수를 삽입할 수 있는 강력한 방법입니다.

```Kotlin
val c = "Kotlin"
println("Hello, $c!") // "Hello, Kotlin!"
```

## 깊이 파헤치기

문자열 연결은 프로그래밍 언어의 역사와 깊이 연관되어 있습니다. 초기 프로그래밍 언어에서는 이 작업이 단순하지 않았지만, Kotlin에서는 문자열 연결을 쉽게 할 수 있도록 많은 도구를 제공하고 있습니다.

문자열 연결의 대체 방법으로 "StringBuilder"를 사용할 수 있습니다. 이러한 접근 방법은 긴 문자열을 반복적으로 생성할 때 성능을 향상시키는 데 유용합니다.

구현 세부사항으로는 Kotlin의 내부에서 연산자 오버로딩이 어떻게 작동하는지, 문자열 템플릿이 어떻게 처리되는지 등을 알 수 있습니다. 이는 컴파일 타임에 문자열 연결 코드가 최적화되는 방식을 이해하는 데 도움이 됩니다.

## 참고 또한 

1. [Kotlin 문자열 템플릿](https://kotlinlang.org/docs/basic-syntax.html)
2. [Kotlin에서의 문자열 관리](https://kotlinlang.org/docs/idioms.html#string-interpolation)
3. [Stackoverflow: Kotlin에서 효율적으로 문자열 연결](https://stackoverflow.com/questions/46450220/when-should-i-use-the-stringbuilder-in-kotlin)