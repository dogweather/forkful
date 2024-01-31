---
title:                "Lavorare con YAML"
date:                  2024-01-19
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"

category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML sta per "YAML Ain't Markup Language" ed è un formato per serializzare dati facilmente leggibile dagli umani. I programmatori lo usano perché è semplice da scrivere e comprendere, ed è comunemente adoperato per file di configurazione e dati interscambiabili.

## How to:
In Kotlin, puoi gestire YAML con librerie come `snakeyaml` o `kotlinx.serialization`. Ecco un esempio semplice che mostra come leggere e scrivere YAML.

```Kotlin
import org.yaml.snakeyaml.Yaml
import java.io.File

fun main() {
    val yaml = Yaml()
    val data: Map<String, Any> = yaml.load(File("config.yml").inputStream())

    println("Server: ${data["server"]}")
    println("Port: ${data["port"]}")

    val newYamlData = mapOf("server" to "localhost", "port" to 8080)
    val yamlString = yaml.dump(newYamlData)
    File("new_config.yml").writeText(yamlString)
}
```
Output:
```
Server: example.com
Port: 80
```

## Deep Dive
YAML è in uso dal 2001, come alternativa leggibile a XML. Mentre JSON è spesso considerato analogo, YAML permette commenti e supporta riferimenti a parti dello stesso YAML, rendendolo più potente per alcune configurazioni complesse. Molte librerie di parsing YAML in Kotlin sono wrapper di librerie Java a causa dell'interoperabilità JVM. È fondamentale gestire possibili eccezioni di parsing YAML.

## See Also
- YAML ufficiale: https://yaml.org
- Libreria SnakeYAML: https://bitbucket.org/asomov/snakeyaml
- modulo `kotlinx.serialization` per YAML: https://github.com/Kotlin/kotlinx.serialization
- Tutorial YAML per principianti: https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/
