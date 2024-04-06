---
date: 2024-01-20 17:39:04.745706-07:00
description: "How to: (\uBC29\uBC95) Kotlin\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uC18C\
  \uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30\uB294 \uAC04\uB2E8\uD569\uB2C8\uB2E4\
  . `toLowerCase()` \uD568\uC218\uB97C \uBB38\uC790\uC5F4\uC5D0 \uC801\uC6A9\uD558\
  \uBA74 \uB429\uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uC5B4\uB5BB\uAC8C \uD558\uB294\uC9C0\
  \uC5D0 \uB300\uD55C \uC608\uC2DC\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.899240-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) Kotlin\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\
  \uB85C \uBCC0\uD658\uD558\uAE30\uB294 \uAC04\uB2E8\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 4
---

## How to: (방법)
Kotlin에서 문자열을 소문자로 변환하기는 간단합니다. `toLowerCase()` 함수를 문자열에 적용하면 됩니다. 다음은 어떻게 하는지에 대한 예시입니다:

```Kotlin
fun main() {
    val originalString = "Hello, World!"
    val lowerCaseString = originalString.toLowerCase()
    
    println(lowerCaseString)  // "hello, world!"
}
```

이 코드는 "Hello, World!"라는 문자열을 "hello, world!"로 변환하여 출력합니다.

## Deep Dive (심층 분석)
문자열을 소문자로 변환하는 작업은 프로그래밍 언어에서 오래전부터 제공되었습니다. 이 기능의 중요성은 컴퓨터가 대소문자를 구분해서 저장하기 때문입니다. 

대안으로 `lowercase()` 함수가 Kotlin 1.5부터 도입되었으며, `toLowerCase()` 함수와 유사하게 작동하나 Locale을 고려하는 차이점이 있습니다. Locale을 고려하지 않고 전환하려면 `lowercase(Locale.ROOT)`와 같이 사용하면 됩니다.

```Kotlin
fun main() {
    val originalString = "İstanbul"
    val lowerCaseStringWithLocale = originalString.lowercase(Locale("tr", "TR"))
    val lowerCaseStringWithoutLocale = originalString.lowercase(Locale.ROOT)

    println(lowerCaseStringWithLocale)  // "istanbul"
    println(lowerCaseStringWithoutLocale)  // "i̇stanbul"
}
```

특정 언어(예: 터키어)에서는 특정 문자들이 다른 언어와 변환 규칙이 다를 수 있으니, Locale을 고려한 변환을 해야 텍스트가 올바르게 처리됩니다.

## See Also (함께 보기)
- Kotlin 공식 문서 [`lowercase()`와 `toLowerCase()` 관련 설명](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/lowercase.html)
- Oracle Java Docs [`Locale` 클래스 설명](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)
- Unicode 문자열 처리에 대한 팁과 트릭 [Unicode Consortium](https://unicode.org)
