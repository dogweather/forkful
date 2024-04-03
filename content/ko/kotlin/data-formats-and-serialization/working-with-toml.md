---
date: 2024-01-26 04:24:08.069736-07:00
description: "TOML\uC740 Tom's Obvious, Minimal Language\uC758 \uC57D\uC790\uC785\uB2C8\
  \uB2E4. \uC0AC\uB78C\uC774 \uC77D\uACE0 \uC4F0\uAE30 \uC26C\uC6B0\uBA74\uC11C\uB3C4\
  \ \uAE30\uACC4\uAC00 \uD30C\uC2F1\uD558\uAE30 \uC26C\uC6CC \uC124\uC815 \uD30C\uC77C\
  \uC5D0 \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uAC1C\uBC1C\uC790\uB4E4\uC740 \uC124\uC815\
  \uC744 \uCC98\uB9AC\uD560 \uB54C XML\uC758 \uBCF5\uC7A1\uD568\uACFC JSON\uC758 \uAE4C\
  \uB2E4\uB85C\uC6C0\uC744 \uD53C\uD558\uAE30 \uC704\uD574 TOML\uC744 \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.206333-06:00'
model: gpt-4-0125-preview
summary: "TOML\uC740 Tom's Obvious, Minimal Language\uC758 \uC57D\uC790\uC785\uB2C8\
  \uB2E4."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
weight: 39
---

## 무엇 & 왜?
TOML은 Tom's Obvious, Minimal Language의 약자입니다. 사람이 읽고 쓰기 쉬우면서도 기계가 파싱하기 쉬워 설정 파일에 사용됩니다. 개발자들은 설정을 처리할 때 XML의 복잡함과 JSON의 까다로움을 피하기 위해 TOML을 사용합니다.

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
