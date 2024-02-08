---
title:                "JSON과 함께 일하기"
date:                  2024-02-03T19:23:25.948253-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON과 함께 일하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
Kotlin에서 JSON(JavaScript Object Notation)을 사용하는 것은 JSON 데이터를 파싱하고 생성하는 것을 포함합니다. 프로그래머는 JSON의 경량이며 읽기 쉬운 형식 덕분에 애플리케이션의 서로 다른 계층 사이 또는 웹 서비스와 통신할 때 데이터를 쉽게 교환하기 위해 이 작업을 수행합니다.

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
