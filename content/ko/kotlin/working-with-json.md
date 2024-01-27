---
title:                "JSON 다루기"
date:                  2024-01-19
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON은 데이터 교환 포맷입니다. 프로그래머들이 데이터 저장, 구조화, 서버와 클라이언트 간 통신을 쉽게 하기 위해 JSON을 사용합니다.

## How to:
Kotlin에서 JSON 다루기 위해 `kotlinx.serialization` 라이브러리를 사용합니다. 아래 예제에서는 JSON 객체를 파싱하는 방법을 보여줍니다.

```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class User(val name: String, val age: Int)

fun parseJson(jsonString: String): User {
    val json = Json { ignoreUnknownKeys = true }
    return json.decodeFromString(User.serializer(), jsonString)
}

fun main() {
    val jsonString = """{"name":"John", "age":30}"""
    val user = parseJson(jsonString)
    println(user)
}
```
출력:
```
User(name=John, age=30)
```

## Deep Dive
JSON, JavaScript Object Notation의 줄임말로, 경량 데이터 교환 포맷입니다. 2001년에 더글라스 크록포드(Douglas Crockford)가 제안했습니다. XML, YAML 같은 대안이 있지만, JSON은 읽기 쉽고 모든 주요 프로그래밍 언어에서 널리 지원됩니다. Kotlin에서는 `kotlinx.serialization`를 포함한 여러 라이브러리가 JSON 파싱 및 직렬화를 지원합니다.

## See Also
- Kotlin Serialization 공식 문서: [https://kotlinlang.org/docs/serialization.html](https://kotlinlang.org/docs/serialization.html)
- JSON 공식 사이트: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
- `kotlinx.serialization` GitHub 페이지: [https://github.com/Kotlin/kotlinx.serialization](https://github.com/Kotlin/kotlinx.serialization)
