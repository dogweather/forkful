---
date: 2024-01-26 04:24:08.069736-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Kotlin\uC5D0\uC11C TOML\uC744 \uB2E4\uB8E8\
  \uB824\uBA74 `ktoml`\uACFC \uAC19\uC740 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\
  \uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC6B0\uC120, `build.gradle.kts`\uC5D0\
  \ \uC758\uC874\uC131\uC744 \uCD94\uAC00\uD569\uC2DC\uB2E4."
lastmod: '2024-03-13T22:44:55.206333-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\uC5D0\uC11C TOML\uC744 \uB2E4\uB8E8\uB824\uBA74 `ktoml`\uACFC \uAC19\
  \uC740 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
weight: 39
---

## 사용 방법:
Kotlin에서 TOML을 다루려면 `ktoml`과 같은 라이브러리를 사용할 수 있습니다. 우선, `build.gradle.kts`에 의존성을 추가합시다:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

이제, TOML을 파싱해봅시다:

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val tomlContent = TomlFileReader.readAndParseFile("config.toml")
    
    val databaseConfig = tomlContent.getTable("database")
    val host = databaseConfig.getString("host")
    val port = databaseConfig.getLong("port")

    println("데이터베이스 호스트: $host")
    println("데이터베이스 포트: $port")
}
```

`config.toml`이 이렇게 생겼다고 가정해봅시다:

```toml
[database]
host = "localhost"
port = 5432
```

샘플 출력은 다음과 같을 것입니다:

```
데이터베이스 호스트: localhost
데이터베이스 포트: 5432
```

## 심층 분석
2013년 GitHub 공동 창립자인 Tom Preston-Werner에 의해 개발된 TOML은 YAML보다 직관적이고 JSON보다 타입 안전성이 높다는 목표를 가졌습니다. Rust의 `Cargo`와 Go의 모듈 시스템에서 특히 인기를 얻었습니다. 대안은 무엇인가요? YAML은 더 많은 기능을 가지고 있고, JSON은 많은 코딩 언어에서 객체로 직접 변환되며, 항상 좋은 구식 XML도 있습니다. 구현에 대해서는, Apache 2.0 라이센스 아래 있는 ktoml은 순수 Kotlin 라이브러리이며 Java 라이브러리를 끌고 오지 않고, 읽는 것뿐만 아니라 TOML을 작성할 수 있는 DSL도 제공합니다.

## 참고 자료
- TOML GitHub: https://github.com/toml-lang/toml
- ktoml GitHub: https://github.com/akuleshov7/ktoml
- TOML vs. YAML vs. JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
