---
title:                "부분 문자열 추출"
aliases:
- /ko/kotlin/extracting-substrings/
date:                  2024-01-20T17:46:03.772880-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
(무엇 & 왜?)
문자열에서 필요한 부분만 뽑아내는 것을 서브스트링 추출이라고 합니다. 데이터를 정제하고, 필요한 정보만을 가져오기 위해 프로그래머들이 이 방법을 자주 사용합니다.

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
