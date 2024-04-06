---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:38.311487-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Kotlin\uC5D0\uC11C \uD2B9\uC815 \uD328\uD134\
  \uACFC \uBB38\uC790\uC5F4\uC774 \uB9E4\uCE58\uB418\uB294\uC9C0 \uD655\uC778\uD558\
  \uB824\uBA74 `Regex` \uD074\uB798\uC2A4\uC758 `matches` \uBA54\uC11C\uB4DC\uB97C\
  \ \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.157497-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\uC5D0\uC11C \uD2B9\uC815 \uD328\uD134\uACFC \uBB38\uC790\uC5F4\uC774\
  \ \uB9E4\uCE58\uB418\uB294\uC9C0 \uD655\uC778\uD558\uB824\uBA74 `Regex` \uD074\uB798\
  \uC2A4\uC758 `matches` \uBA54\uC11C\uB4DC\uB97C \uC0AC\uC6A9\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
weight: 11
---

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
