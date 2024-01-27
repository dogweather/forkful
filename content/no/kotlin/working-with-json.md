---
title:                "Arbeid med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Jobbing med JSON (JavaScript Object Notation) handler om datautveksling. Programmerere bruker JSON for å lagre og utveksle data på en lettleselig måte for både mennesker og maskiner.

## Hvordan:
I Kotlin kan du enkelt parse og generere JSON ved hjelp av biblioteket kotlinx.serialization. Først må du legge til avhengighet i `build.gradle.kts`:

```kotlin
dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2")
}
```

Så kan du parse JSON til en Kotlin-klasse og omvendt:

```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// Definer en dataklasse
@Serializable
data class Bruker(val navn: String, val alder: Int)

fun main() {
    // JSON-tekst
    val jsonStr = """{"navn": "Ola", "alder": 30}"""

    // Parse JSON til Kotlin-objekt
    val bruker = Json.decodeFromString<Bruker>(jsonStr)
    println(bruker) // Output: Bruker(navn=Ola, alder=30)

    // Generer JSON fra Kotlin-objekt
    val jsonOut = Json.encodeToString(bruker)
    println(jsonOut) // Output: {"navn":"Ola","alder":30}
}
```

## Deep Dive
JSON ble introdusert i 2001 og har raskt blitt standard for nettapplikasjoner. Alternativer til JSON inkluderer XML og YAML, men disse er ofte mer verbøse. kotlinx.serialization er laget for Kotlin og integrerer sømløst med Kotlin korutiner og andre språkfunksjoner, som dataklasser og immutabilitet.

## Se Også
For mer om JSON og Kotlin serialisering:

- kotlinx.serialization GitHub: https://github.com/Kotlin/kotlinx.serialization
- Offisiell Kotlin serialization guide: https://kotlinlang.org/docs/serialization.html
- JSON format spesifikasjon: https://www.json.org/json-en.html
