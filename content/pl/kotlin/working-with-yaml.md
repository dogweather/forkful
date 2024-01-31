---
title:                "Praca z yaml"
date:                  2024-01-19
simple_title:         "Praca z yaml"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Czym jest i po co się z tym męczyć? YAML, czyli "YAML Ain't Markup Language", to ludzki format do danych konfiguracyjnych. Programiści używają go dla przejrzystości i prostej składni, co ułatwia zarządzanie konfiguracjami i danymi.

## How to:
Aby pracować z YAML w Kotlinie, użyjemy biblioteki `snakeyaml`. Oto prosty przykład:

```Kotlin
import org.yaml.snakeyaml.Yaml
import java.io.File

fun main() {
    val yaml = Yaml()
    val configData = """
        mysql:
          host: localhost
          port: 3306
          user: root
          password: pass
    """.trimIndent()

    val loadConfig = yaml.load<Map<String, Map<String, Any>>>(configData)
    println("MySQL Host: ${loadConfig["mysql"]?.get("host")}")
}
```

Wyjście:
```
MySQL Host: localhost
```

Instalacja `snakeyaml` przez Gradle:

```Groovy
dependencies {
    implementation("org.yaml:snakeyaml:1.29")
}
```

## Deep Dive:
YAML bywa mylnie mylony z językiem znaczników, ale to format serializacji danych. Pojawił się w 2001 roku jako alternatywa dla XML. Inne opcje to JSON czy TOML, które również łączą łatwą składnię z elastycznością. W Kotlinie pracuje się z YAML przy użyciu dodatkowych bibliotek jak `snakeyaml` lub `kotlinx.serialization` z obsługą formatu YAML.

## See Also:
- Specyfikacja YAML: https://yaml.org/spec/1.2/spec.html
- Dokumentacja SnakeYAML: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation
- kotlinx.serialization z obsługą YAML: https://github.com/charleskorn/kaml
