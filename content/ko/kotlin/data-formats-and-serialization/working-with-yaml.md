---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:01.387356-07:00
description: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uB85C,\
  \ \uAD6C\uC131 \uD30C\uC77C, \uB370\uC774\uD130 \uC800\uC7A5, \uD504\uB85C\uC138\
  \uC2A4 \uAC04 \uBA54\uC2DC\uC9D5\uC5D0 \uC790\uC8FC \uC0AC\uC6A9\uB418\uB294 \uB9E4\
  \uC6B0 \uC77D\uAE30 \uC26C\uC6B4 \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uD615\uC2DD\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uAD6C\uC131 \uBC0F\
  \ \uC124\uC815\uC744 \uAD6C\uC870\uC801\uC774\uBA74\uC11C\uB3C4 \uAC04\uB2E8\uD55C\
  \ \uBC29\uC2DD\uC73C\uB85C \uAD00\uB9AC\uD558\uAE30 \uC704\uD574 \uC885\uC885 YAML\uC744\
  \u2026"
lastmod: '2024-02-25T18:49:52.198495-07:00'
model: gpt-4-0125-preview
summary: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uB85C, \uAD6C\
  \uC131 \uD30C\uC77C, \uB370\uC774\uD130 \uC800\uC7A5, \uD504\uB85C\uC138\uC2A4 \uAC04\
  \ \uBA54\uC2DC\uC9D5\uC5D0 \uC790\uC8FC \uC0AC\uC6A9\uB418\uB294 \uB9E4\uC6B0 \uC77D\
  \uAE30 \uC26C\uC6B4 \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uD615\uC2DD\uC785\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uAD6C\uC131 \uBC0F \uC124\uC815\
  \uC744 \uAD6C\uC870\uC801\uC774\uBA74\uC11C\uB3C4 \uAC04\uB2E8\uD55C \uBC29\uC2DD\
  \uC73C\uB85C \uAD00\uB9AC\uD558\uAE30 \uC704\uD574 \uC885\uC885 YAML\uC744\u2026"
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
YAML은 "YAML Ain't Markup Language"의 약자로, 구성 파일, 데이터 저장, 프로세스 간 메시징에 자주 사용되는 매우 읽기 쉬운 데이터 직렬화 형식입니다. 프로그래머들은 구성 및 설정을 구조적이면서도 간단한 방식으로 관리하기 위해 종종 YAML을 사용하며, 읽기 쉬움이 중요할 때 JSON이나 XML보다 그 명확성과 간단함으로부터 이점을 얻습니다.

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
