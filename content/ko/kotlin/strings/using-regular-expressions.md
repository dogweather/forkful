---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:38.311487-07:00
description: "\uC815\uADDC \uD45C\uD604\uC2DD(regex)\uC740 \uD14D\uC2A4\uD2B8 \uCC98\
  \uB9AC\uC5D0 \uC788\uC5B4 \uAC15\uB825\uD55C \uB3C4\uAD6C\uB85C, \uD504\uB85C\uADF8\
  \uB798\uBA38\uAC00 \uACE0\uAE09 \uD328\uD134 \uB9E4\uCE6D \uAE30\uC220\uC744 \uC0AC\
  \uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\uC744 \uAC80\uC0C9, \uB9E4\uCE58, \uC870\uC791\
  \uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. Kotlin\uC5D0\uC11C \uC815\uADDC\
  \ \uD45C\uD604\uC2DD\uC744 \uD65C\uC6A9\uD558\uBA74 \uAC80\uC99D, \uD30C\uC2F1,\
  \ \uBCC0\uD658\uACFC \uAC19\uC740 \uBCF5\uC7A1\uD55C \uD14D\uC2A4\uD2B8 \uCC98\uB9AC\
  \ \uC791\uC5C5\uC744 \uD6A8\uC728\uC801\uC73C\uB85C \uC218\uD589\uD560 \uC218\u2026"
lastmod: 2024-02-19 22:05:14.069018
model: gpt-4-0125-preview
summary: "\uC815\uADDC \uD45C\uD604\uC2DD(regex)\uC740 \uD14D\uC2A4\uD2B8 \uCC98\uB9AC\
  \uC5D0 \uC788\uC5B4 \uAC15\uB825\uD55C \uB3C4\uAD6C\uB85C, \uD504\uB85C\uADF8\uB798\
  \uBA38\uAC00 \uACE0\uAE09 \uD328\uD134 \uB9E4\uCE6D \uAE30\uC220\uC744 \uC0AC\uC6A9\
  \uD558\uC5EC \uBB38\uC790\uC5F4\uC744 \uAC80\uC0C9, \uB9E4\uCE58, \uC870\uC791\uD560\
  \ \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. Kotlin\uC5D0\uC11C \uC815\uADDC\
  \ \uD45C\uD604\uC2DD\uC744 \uD65C\uC6A9\uD558\uBA74 \uAC80\uC99D, \uD30C\uC2F1,\
  \ \uBCC0\uD658\uACFC \uAC19\uC740 \uBCF5\uC7A1\uD55C \uD14D\uC2A4\uD2B8 \uCC98\uB9AC\
  \ \uC791\uC5C5\uC744 \uD6A8\uC728\uC801\uC73C\uB85C \uC218\uD589\uD560 \uC218\u2026"
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜 사용하는가?

정규 표현식(regex)은 텍스트 처리에 있어 강력한 도구로, 프로그래머가 고급 패턴 매칭 기술을 사용하여 문자열을 검색, 매치, 조작할 수 있게 해줍니다. Kotlin에서 정규 표현식을 활용하면 검증, 파싱, 변환과 같은 복잡한 텍스트 처리 작업을 효율적으로 수행할 수 있으며, 간단한 문자열 조작부터 복잡한 텍스트 분석에 이르기까지 필수적인 작업을 수행할 수 있습니다.

## 사용 방법:

### 기본 매칭
Kotlin에서 특정 패턴과 문자열이 매치되는지 확인하려면 `Regex` 클래스의 `matches` 메서드를 사용할 수 있습니다.

```kotlin
val pattern = "kotlin".toRegex()
val input = "I love kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // 출력 결과: true
```

### 문자열 부분 찾기 및 추출하기
패턴과 매치되는 문자열의 부분을 찾고자 할 때, Kotlin은 모든 매치를 순회할 수 있게 해줍니다:

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "오늘 날짜는 07/09/2023입니다."
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// 출력 결과: 07/09/2023
```

### 텍스트 교체하기
패턴과 매치되는 문자열의 부분을 교체하는 것은 `replace` 함수를 사용하면 간단합니다:

```kotlin
val input = "Username: user123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // 출력 결과: Username: userXXX
```

### 문자열 분할하기
정규 표현식 패턴을 구분자로 사용하여 문자열을 리스트로 분할하기:

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // 출력 결과: [1, 2, 3, 4, 5]
```

### 제3자 라이브러리: Kotest
[Kotest](https://github.com/kotest/kotest)는 Kotlin의 내장 정규 표현식 지원을 확장하는 인기 있는 Kotlin 테스트 라이브러리로, 특히 테스트 케이스에서의 검증에 유용합니다.

```kotlin
// 프로젝트에 Kotest가 추가되었다고 가정
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@test.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// 이것은 입력이 이메일 패턴과 매치되면 테스트를 통과합니다.
```

Kotlin 애플리케이션에 정규 표현식을 통합함으로써, 효율적으로 고급 텍스트 처리를 수행할 수 있습니다. 사용자 입력을 검증하거나, 데이터를 추출하거나, 문자열을 변환하는 등, 정규 표현식 패턴은 강력한 해결책을 제공합니다.
