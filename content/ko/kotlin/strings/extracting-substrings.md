---
date: 2024-01-20 17:46:03.772880-07:00
description: "How to: (\uC2EC\uCE35 \uD0D0\uAD6C) \uC11C\uBE0C\uC2A4\uD2B8\uB9C1 \uCD94\
  \uCD9C\uC740 \uBB38\uC790\uC5F4 \uCC98\uB9AC\uC758 \uAE30\uCD08\uC785\uB2C8\uB2E4\
  . \uC774 \uAE30\uB2A5\uC740 \uC6D0\uC2DC \uBB38\uC790\uC5F4\uC744 \uB2E4\uB8F0 \uB54C\
  \uBD80\uD130 \uC788\uC5C8\uACE0 \uB9CE\uC740 \uD504\uB85C\uADF8\uB798\uBC0D \uC5B8\
  \uC5B4\uAC00 \uC774\uB97C \uC9C0\uC6D0\uD569\uB2C8\uB2E4. Kotlin \uC5D0\uC11C\uB294\
  \ `substring` \uD568\uC218\uC640 \uBC94\uC704 \uC5F0\uC0B0\uC790\uB97C \uD1B5\uD574\
  \ \uAD6C\uD604\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. `substring` \uD568\uC218\uB97C\
  \u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.901155-06:00'
model: gpt-4-1106-preview
summary: "(\uC2EC\uCE35 \uD0D0\uAD6C) \uC11C\uBE0C\uC2A4\uD2B8\uB9C1 \uCD94\uCD9C\uC740\
  \ \uBB38\uC790\uC5F4 \uCC98\uB9AC\uC758 \uAE30\uCD08\uC785\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C"
weight: 6
---

## How to:
(어떻게 하나요?)
```kotlin
fun main() {
    val fullString = "안녕하세요, 여러분"
    val extracted = fullString.substring(4, 9)
    
    println(extracted) // 출력: 세요, 
}

fun getRangeFromText(startText: String, endText: String, fullText: String): String? {
    val startIndex = fullText.indexOf(startText) + startText.length
    val endIndex = fullText.indexOf(endText, startIndex)
    
    return if (startIndex > 0 && endIndex > 0) fullText.substring(startIndex, endIndex) else null
}

fun main() {
    val startText = "안녕"
    val endText = "분"
    val fullText = "안녕하세요, 여러분"
    val result = getRangeFromText(startText, endText, fullText)
    
    println(result) // 출력: 하세요, 여러
}
```

## Deep Dive:
(심층 탐구)
서브스트링 추출은 문자열 처리의 기초입니다. 이 기능은 원시 문자열을 다룰 때부터 있었고 많은 프로그래밍 언어가 이를 지원합니다. Kotlin 에서는 `substring` 함수와 범위 연산자를 통해 구현할 수 있습니다. `substring` 함수를 사용할 때는 시작 인덱스와 종료 인덱스를 명시하여 원하는 부분을 추출합니다. 종료 인덱스는, 그 자리 문자는 포함하지 않습니다. 인덱스 계산을 실수로 하지 않도록 주의하세요. 문자열 처리에는 정규식이라는 강력한 대안이 있지만, 더 복잡할 수 있습니다. `indexOf` 함수를 사용해 특정 문자나 문자열의 위치를 찾은 다음, 서브스트링을 추출하는 것도 가능한 방법 중 하나입니다.

## See Also:
(관련 자료)
- Kotlin 공식 문서의 문자열 처리 섹션: [Kotlin String Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
