---
title:                "Travailler avec YAML"
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
On manipule YAML pour configurer des projets et échanger des données. C'est lisible et facile à comprendre, donc les programmeurs l'adorent.

## How to:
```kotlin
import com.fasterxml.jackson.module.kotlin.readValue
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper

// Un YAML simple
val yamlContent = """
    - name: "John Doe"
      age: 30
    - name: "Jane Smith"
      age: 25
"""

// Transforme le YAML en liste
val mapper = YAMLMapper()
val people: List<Map<String, Any>> = mapper.readValue(yamlContent)

// Affichage
println(people)
```
*Output:*
```
[{name=John Doe, age=30}, {name=Jane Smith, age=25}]
```

## Deep Dive
YAML, "YAML Ain't Markup Language", est né en 2001. Alternatives: JSON, moins lisible, XML, plus verbeux. Kotlin utilise des librairies comme Jackson pour intégrer facilement le YAML.

## See Also
- YAML officiel: https://yaml.org
- Jackson YAML module GitHub: https://github.com/FasterXML/jackson-dataformat-yaml
- Kotlin langage doc: https://kotlinlang.org/docs/reference/
