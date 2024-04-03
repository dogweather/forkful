---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:01.387356-07:00
description: "\uBC29\uBC95: Kotlin\uC740 YAML \uD30C\uC2F1 \uBC0F \uC9C1\uB82C\uD654\
  \uB97C \uC704\uD55C \uB0B4\uC7A5 \uC9C0\uC6D0\uC744 \uC81C\uACF5\uD558\uC9C0 \uC54A\
  \uC9C0\uB9CC, \uC77C\uBC18 YAML \uD30C\uC2F1\uC744 \uC704\uD55C `snakeyaml`\uACFC\
  \ YAML \uD615\uC2DD \uD655\uC7A5\uC744 \uC0AC\uC6A9\uD558\uB294 `kotlinx.serialization`\uACFC\
  \ \uAC19\uC740 \uC778\uAE30 \uC788\uB294 \uD0C0\uC0AC \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB97C \uC0AC\uC6A9\uD558\uC5EC YAML \uD30C\uC77C\uC744 \uC791\uC5C5\uD560\u2026"
lastmod: '2024-03-13T22:44:55.201407-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\uC740 YAML \uD30C\uC2F1 \uBC0F \uC9C1\uB82C\uD654\uB97C \uC704\uD55C\
  \ \uB0B4\uC7A5 \uC9C0\uC6D0\uC744 \uC81C\uACF5\uD558\uC9C0 \uC54A\uC9C0\uB9CC, \uC77C\
  \uBC18 YAML \uD30C\uC2F1\uC744 \uC704\uD55C `snakeyaml`\uACFC YAML \uD615\uC2DD\
  \ \uD655\uC7A5\uC744 \uC0AC\uC6A9\uD558\uB294 `kotlinx.serialization`\uACFC \uAC19\
  \uC740 \uC778\uAE30 \uC788\uB294 \uD0C0\uC0AC \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC YAML \uD30C\uC77C\uC744 \uC791\uC5C5\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

## 방법:
Kotlin은 YAML 파싱 및 직렬화를 위한 내장 지원을 제공하지 않지만, 일반 YAML 파싱을 위한 `snakeyaml`과 YAML 형식 확장을 사용하는 `kotlinx.serialization`과 같은 인기 있는 타사 라이브러리를 사용하여 YAML 파일을 작업할 수 있습니다.

### `snakeyaml` 사용하기
**의존성:**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**YAML 읽기:**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// 샘플 사용법
fun main() {
    readYaml("config.yaml")
}
```
**샘플 `config.yaml`:**
```yaml
database:
  host: localhost
  port: 5432
```
**샘플 출력:**
```
{database={host=localhost, port=5432}}
```

### `kotlinx.serialization`과 함께 YAML 사용하기
먼저, 적합한 YAML 지원 라이브러리가 포함된 `kotlinx-serialization` 라이브러리를 갖추었는지 확인하세요(사용 가능하다면, `kotlinx.serialization`은 주로 JSON 및 기타 형식을 직접 대상으로 합니다).

**의존성:**
```kotlin
// JSON을 위한 것(설명용, YAML 지원이나 대체 라이브러리 확인)
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**직렬화 가능한 데이터 클래스 정의하기:**
```kotlin
import kotlinx.serialization.Serializable

@Serializable
data class Config(
    val database: Database
)

@Serializable
data class Database(
    val host: String,
    val port: Int
)
```

글을 쓰는 시점에서, `kotlinx.serialization`에서의 직접적인 YAML 지원은 제한적일 수 있거나 발전 중일 수 있습니다. `snakeyaml`을 사용하여 YAML을 JSON으로 변환한 다음 `kotlinx.serialization`으로 JSON을 파싱하는 중간 표현을 사용해야 하거나 `kotlinx.serialization`과 호환되는 커뮤니티 주도의 YAML 직렬화 프로젝트를 찾아야 할 수도 있습니다.

JSON의 경우 코드는 다음과 같습니다:
```kotlin
import kotlinx.serialization.json.Json
import kotlinx.serialization.decodeFromString

fun main() {
    val jsonText = """
    {
        "database": {
            "host": "localhost",
            "port": 5432
        }
    }
    """.trimIndent()
    
    val config = Json.decodeFromString<Config>(jsonText)
    println(config)
}
```

Kotlin과 그 생태계가 계속 발전함에 따라, YAML 지원 및 라이브러리의 최신 소식을 위해 공식 문서와 커뮤니티 리소스를 주시하세요.
