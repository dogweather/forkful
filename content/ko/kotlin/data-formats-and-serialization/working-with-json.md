---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:25.948253-07:00
description: "\uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB098\uC694: Kotlin\uC740 JSON\uC744\
  \ \uC704\uD55C \uB0B4\uC7A5 \uC9C0\uC6D0\uC744 \uD3EC\uD568\uD558\uC9C0 \uC54A\uC9C0\
  \uB9CC `Gson` (Google \uC81C\uACF5) \uBC0F `Kotlinx.serialization` (JetBrains \uC81C\
  \uACF5)\uACFC \uAC19\uC740 \uD0C0\uC0AC \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC758 \uAC15\
  \uB825\uD55C \uAE30\uB2A5\uC744 \uD65C\uC6A9\uD569\uB2C8\uB2E4. \uB2E4\uC74C\uC740\
  \ JSON\uC744 \uC0AC\uC6A9\uD558\uAE30 \uC704\uD574 \uB450\u2026"
lastmod: '2024-03-13T22:44:55.203053-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\uC740 JSON\uC744 \uC704\uD55C \uB0B4\uC7A5 \uC9C0\uC6D0\uC744 \uD3EC\
  \uD568\uD558\uC9C0 \uC54A\uC9C0\uB9CC `Gson` (Google \uC81C\uACF5) \uBC0F `Kotlinx.serialization`\
  \ (JetBrains \uC81C\uACF5)\uACFC \uAC19\uC740 \uD0C0\uC0AC \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uC758 \uAC15\uB825\uD55C \uAE30\uB2A5\uC744 \uD65C\uC6A9\uD569\uB2C8\uB2E4\
  ."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

## 어떻게 사용하나요:
Kotlin은 JSON을 위한 내장 지원을 포함하지 않지만 `Gson` (Google 제공) 및 `Kotlinx.serialization` (JetBrains 제공)과 같은 타사 라이브러리의 강력한 기능을 활용합니다. 다음은 JSON을 사용하기 위해 두 가지 라이브러리 모두를 사용하는 방법입니다.

### Gson 사용하기
`build.gradle` 파일에 Gson 의존성을 추가합니다:
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

JSON 문자열을 객체로 파싱하고 그 반대로 하는 방법:
```kotlin
import com.google.gson.Gson

// 데이터 클래스 정의
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // 직렬화
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // 출력: {"name":"John Doe","age":30}

    // 역직렬화
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // 출력: User(name=John Doe, age=30)
}
```

### Kotlinx.serialization 사용하기
먼저, `build.gradle`에 의존성을 포함합니다:
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

그 후, 빌드 스크립트 상단에 `kotlinx-serialization` 플러그인을 적용합니다:
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

Kotlinx.serialization을 사용한 직렬화 및 역직렬화:
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// 직렬화 가능한 데이터 클래스 정의
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // 직렬화
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // 출력: {"name":"Jane Doe","age":28}

    // 역직렬화
    val user = Json.decodeFromString<User>(json)
    println(user)  // 출력: User(name=Jane Doe, age=28)
}
```

Gson과 Kotlinx.serialization 모두 Kotlin 애플리케이션에서 JSON을 사용하는 것을 단순화하지만, 어느 것을 선택할지는 특정 프로젝트 요구사항과 개인적인 선호도에 따라 달라집니다.
