---
title:                "프로그래머를 위한 TOML 다루기"
aliases:
- ko/kotlin/working-with-toml.md
date:                  2024-01-26T04:24:08.069736-07:00
model:                 gpt-4-0125-preview
simple_title:         "프로그래머를 위한 TOML 다루기"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/working-with-toml.md"
---

{{< edit_this_page >}}

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
