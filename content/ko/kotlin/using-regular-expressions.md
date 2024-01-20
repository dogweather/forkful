---
title:                "정규 표현식 활용하기"
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜 사용하는가?)
정규 표현식(Regular Expressions)은 문자열에서 패턴을 찾거나 조작하는데 사용합니다. 프로그래머는 코드 내에서 복잡한 문자열 검색, 대체, 검증 작업을 빠르고 효율적으로 처리하기 위해 이를 사용합니다.

## How to: (어떻게 사용하는가?)
```kotlin
fun main() {
    val text = "저는 Kotlin을 사랑합니다!"
    val pattern = "Kotlin".toRegex()

    // 패턴 일치 확인
    println(pattern.containsMatchIn(text)) // 결과: true

    // 일치하는 부분 추출
    val matchResult = pattern.find(text)
    println(matchResult?.value) // 결과: Kotlin

    // 문자열 대체
    val replacedText = text.replace(pattern, "Java")
    println(replacedText) // 결과: 저는 Java을 사랑합니다!
}
```

## Deep Dive (심층 분석)
정규 표현식은 1950년대 초 Stephen Kleene에 의해 개발되었고, 컴퓨팅 작업에서 문자열 처리를 간소화하기 위해 널리 사용됩니다. 대안으로는 문자열 함수(예: `replace`, `substring`)나 외부 라이브러리를 사용할 수 있습니다만, 정규 표현식은 이들보다 더 강력한 패턴 매칭을 제공합니다. Kotlin은 내부적으로 `java.util.regex` 패키지를 사용하여 정규 표현식을 구현합니다.

## See Also (추가 정보)
- [Kotlin 공식 문서](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [정규 표현식 학습](https://www.regular-expressions.info/)
- [정규 표현식 테스터](https://regex101.com/)