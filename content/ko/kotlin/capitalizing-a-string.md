---
title:                "문자열 대문자로 변환하기"
html_title:           "Kotlin: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 뜻과 이유: 
문자열을 대문자로 변환하는 것, 그리고 프로그래머들이 왜 이런 작업을 하는지에 대해 설명합니다.

## 하는 법: 
`Kotlin ...` 코드 블록 안에 코딩 예제와 샘플 출력을 제공합니다.

```
val string = "hello world"
val capitalizedString = string.toUpperCase()
println(capitalizedString)

// Output:
HELLO WORLD
```

## 더 깊이 들어가보기: 
문자열을 대문자로 변환하는 것의 역사적 배경, 대안, 그리고 구현 세부 사항 등에 대해 설명합니다.

### 역사적 배경: 
C 언어에서는 대문자와 소문자의 아스키 코드 값이 단지 32만큼 차이나기 때문에 소문자로 된 문자열을 매번 대문자로 변환하는 것은 간단한 작업이었습니다. 하지만 자바에서는 이를 좀 더 우아하게 처리할 수 있는 `toUpperCase()` 메소드를 제공하기 시작했습니다. 현재 Kotlin에서도 이를 지원하며, 보다 간단하고 효율적인 방법으로 문자열을 대문자로 변환할 수 있습니다.

### 대안: 
Kotlin에서는 문자열을 변환할 때 다양한 방법을 제공하고 있습니다. 대문자는 `toUpperCase()` 메소드를 사용하여 변환할 수 있고, 소문자는 `toLowerCase()` 메소드를 사용하여 변환할 수 있습니다. 또한, `capitalize()` 메소드를 사용하면 문자열의 첫 글자를 대문자로 변환할 수 있습니다. 이 외에도 정규식이나 비트 연산을 사용하여 문자열을 변환하는 방법이 있습니다.

### 구현 세부 사항: 
Kotlin에서 문자열을 대문자로 변환하는 방법은 내부적으로 유니코드를 사용합니다. 즉, 문자열의 각 문자를 대문자로 변환하는 것이 아니라, 유니코드 상에서 대문자에 해당하는 코드 포인트로 변경하여 문자열을 처리합니다. 이를 통해 다국어 문자열을 지원하는데 큰 장점이 있습니다.

## 관련 자료: 
문자열을 변환하는 방법에 대한 관련 정보를 아래 링크에서 확인할 수 있습니다.

- [Kotlin 표준 라이브러리 문서: `toUpperCase()` 메소드](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html)
- [Online Java Tutorials: `toUpperCase()` 메소드](https://www.javatpoint.com/java-string-touppercase)
- [Java String 기본 메소드](https://www.w3schools.com/java/java_ref_string.asp)