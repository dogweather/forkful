---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:02.405743-07:00
description: "YAML, som st\xE5r f\xF6r YAML Ain't Markup Language, \xE4r ett mycket\
  \ l\xE4sligt data-serialiseringsformat som ofta anv\xE4nds f\xF6r konfigurationsfiler,\
  \ datalagring och\u2026"
lastmod: '2024-03-13T22:44:37.891280-06:00'
model: gpt-4-0125-preview
summary: "YAML, som st\xE5r f\xF6r YAML Ain't Markup Language, \xE4r ett mycket l\xE4\
  sligt data-serialiseringsformat som ofta anv\xE4nds f\xF6r konfigurationsfiler,\
  \ datalagring och\u2026"
title: Att Arbeta med YAML
weight: 41
---

## Vad & Varför?
YAML, som står för YAML Ain't Markup Language, är ett mycket läsligt data-serialiseringsformat som ofta används för konfigurationsfiler, datalagring och mellanprocessmeddelanden. Programmerare arbetar ofta med YAML för att hantera konfigurationer och inställningar på ett strukturerat men enkelt sätt, vilket drar nytta av dess klarhet och enkelhet över JSON eller XML när läsbarhet spelar roll.

## Hur man gör:
Kotlin har inte inbyggt stöd för YAML-tolkning och serialisering, men du kan använda populära tredjepartsbibliotek som `snakeyaml` (för allmän YAML-tolkning) och `kotlinx.serialization` (med en YAML-formatutvidgning) för att arbeta med YAML-filer.

### Använda `snakeyaml`
**Beroende:**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**Läs YAML:**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// Exempelanvändning
fun main() {
    readYaml("config.yaml")
}
```
**Exempel `config.yaml`:**
```yaml
database:
  host: localhost
  port: 5432
```
**Exempelutdata:**
```
{database={host=localhost, port=5432}}
```
### Använda `kotlinx.serialization` med YAML
Först, se till att du har biblioteket `kotlinx-serialization` med ett passande YAML-stödbibliotek (om tillgängligt, eftersom `kotlinx.serialization` främst riktar sig till JSON och andra format direkt).

**Beroende:**
```kotlin
// För JSON (illustrativt, kontrollera för YAML-stöd eller alternativa bibliotek)
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**Definiera en serialiserbar dataklass:**
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

Tyvärr, vid tidpunkten för detta skrivande, kan direktstöd för YAML i `kotlinx.serialization` vara begränsat eller under utveckling. Du kan behöva använda en mellanrepresentation (som att konvertera YAML till JSON med `snakeyaml` och sedan tolka JSON med `kotlinx.serialization`) eller leta efter community-drivna YAML-serialiseringsprojekt som är kompatibla med `kotlinx.serialization`.

För JSON skulle koden se ut ungefär så här:
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

Eftersom Kotlin och dess ekosystem fortsätter att utvecklas, håll ett öga på den officiella dokumentationen och communityresurserna för det senaste inom YAML-stöd och bibliotek.
