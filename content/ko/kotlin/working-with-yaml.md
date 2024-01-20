---
title:                "YAML 다루기"
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜 사용하는가?)
YAML은 "YAML Ain't Markup Language"의 약어로, 데이터 직렬화 포맷입니다. 프로그래머들은 설정 파일, 데이터 교환 등을 간편하게 하기 위해 YAML을 사용합니다.

## How to: (어떻게 사용하는가)
Kotlin에서 YAML을 다루려면 외부 라이브러리를 사용해야 합니다. `snakeyaml` 라이브러리를 예로 들어 실습 코드를 보여드리겠습니다.

1. 의존성 추가하기(build.gradle):
```kotlin
dependencies {
    implementation("org.yaml:snakeyaml:1.29")
}
```

2. Kotlin 내에서 YAML 읽기:
```kotlin
import org.yaml.snakeyaml.Yaml

fun main() {
    val yaml = Yaml()
    val document = """
        name: Yoon
        role: Developer
        skills:
          - Kotlin
          - YAML
    """.trimIndent()

    val data = yaml.load<Map<String, Any>>(document)
    println(data)
}
```

출력:
```kotlin
{name=Yoon, role=Developer, skills=[Kotlin, YAML]}
```

3. Kotlin에서 YAML 쓰기:
```kotlin
import org.yaml.snakeyaml.Yaml

fun main() {
    val yaml = Yaml()
    val data = mapOf(
        "name" to "Yoon",
        "role" to "Developer",
        "skills" to listOf("Kotlin", "YAML")
    )

    val output = yaml.dump(data)
    println(output)
}
```

출력:
```yaml
name: Yoon
role: Developer
skills:
- Kotlin
- YAML
```

## Deep Dive (깊게 파기)
YAML은 2001년에 Clark Evans, Ingy döt Net, Oren Ben-Kiki에 의해 생성되었습니다. JSON, XML 같은 다른 데이터 직렬화 포맷에 비해 가독성을 중시합니다. `snakeyaml` 같은 라이브러리로 파싱 및 생성을 수행하지만, Kotlin 환경에서는 `kotlinx.serialization` 모듈과 같은 코틀린-옵티마이즈된 라이브러리들도 있습니다.

## See Also (추가 정보)
- YAML 공식 웹사이트: https://yaml.org
- SnakeYAML 공식 문서: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation
- kotlinx.serialization GitHub: https://github.com/Kotlin/kotlinx.serialization