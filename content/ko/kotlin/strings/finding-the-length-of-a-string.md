---
date: 2024-01-20 17:47:46.780211-07:00
description: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uB294 \uAC83\uC740\
  , \uBB38\uC790\uAC00 \uC5BC\uB9C8\uB098 \uB9CE\uC740\uC9C0 \uC138\uB294 \uAC70\uC608\
  \uC694. \uAE38\uC774\uB97C \uC544\uB294 \uAC83\uC740 \uC785\uB825 \uAC80\uC99D,\
  \ \uBC18\uBCF5, \uBB38\uC790\uC5F4 \uC790\uB974\uAE30 \uAC19\uC740 \uC791\uC5C5\uC744\
  \ \uD560 \uB54C \uD544\uC694\uD574\uC694."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:14.070361
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uB294 \uAC83\uC740, \uBB38\
  \uC790\uAC00 \uC5BC\uB9C8\uB098 \uB9CE\uC740\uC9C0 \uC138\uB294 \uAC70\uC608\uC694\
  . \uAE38\uC774\uB97C \uC544\uB294 \uAC83\uC740 \uC785\uB825 \uAC80\uC99D, \uBC18\
  \uBCF5, \uBB38\uC790\uC5F4 \uC790\uB974\uAE30 \uAC19\uC740 \uC791\uC5C5\uC744 \uD560\
  \ \uB54C \uD544\uC694\uD574\uC694."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열의 길이를 찾는 것은, 문자가 얼마나 많은지 세는 거예요. 길이를 아는 것은 입력 검증, 반복, 문자열 자르기 같은 작업을 할 때 필요해요.

## How to: (어떻게:)
```kotlin
fun main() {
    val greeting = "안녕하세요!"
    println("인사말의 길이: ${greeting.length}")
}

// 샘플 출력:
// 인사말의 길이: 7
```

## Deep Dive (심층 분석)
문자열의 길이를 찾기 위해 Kotlin은 `length` 프로퍼티를 제공합니다. Java에서 상속받은 방법이죠. `length()` 함수 대신 `length` 프로퍼티를 쓰는 겁니다. 예전에는 문자열 크기를 찾을 때 자주 `length()` 함수를 썼어요. Kotlin과 같은 현대 언어는 더 간단하게 만들었죠.

다른 방법으로는 `forEach`, `count`, `fold` 같은 함수를 사용해서 수동으로 길이를 세는 것도 가능하지만, 보통은 필요 없어요. 코틀린에서 문자열 길이는 항상 UTF-16 코드 유닛의 수를 반환하고, 이는 모든 일반 텍스트 문자열 작업에서 충분합니다. 그러나 이모지나 특정 언어의 문자 같은 일부 복합 문자는 두 개 이상의 코드 유닛으로 표현될 수 있어요. 이럴 때는 전체 문자 수와 다를 수 있음을 알아둬야 합니다.

```kotlin
fun main() {
    val specialString = "👩‍👩‍👦‍👦 가족"
    println("특수 문자열 길이: ${specialString.length}")
    
    // 실제 표시되는 문자 개수를 찾고 싶다면 아래의 방법을 사용하세요.
    println("실제 표시되는 문자 개수: ${specialString.codePointCount(0, specialString.length)}")
}

// 샘플 출력:
// 특수 문자열 길이: 10
// 실제 표시되는 문자 개수: 6
```

## See Also (관련 자료)
- Kotlin 공식 문서의 [문자열](https://kotlinlang.org/docs/basic-types.html#strings) 섹션을 찾아보세요.
- 한국어로 Kotlin을 공부하고 싶다면, [코틀린 코리아](http://kotlin.kr/) 사용자 모임이나 [Kotlin Korea Facebook Group](https://www.facebook.com/groups/kotlinkorea/)을 방문해보세요.
