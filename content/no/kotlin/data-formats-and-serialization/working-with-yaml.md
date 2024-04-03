---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:56.337348-07:00
description: "YAML, som st\xE5r for YAML Ain't Markup Language, er et h\xF8yt leselig\
  \ data-serieliseringsformat som ofte brukes til konfigurasjonsfiler, datalagring\
  \ og\u2026"
lastmod: '2024-03-13T22:44:40.772313-06:00'
model: gpt-4-0125-preview
summary: "YAML, som st\xE5r for YAML Ain't Markup Language, er et h\xF8yt leselig\
  \ data-serieliseringsformat som ofte brukes til konfigurasjonsfiler, datalagring\
  \ og meldingsutveksling mellom prosesser."
title: Arbeider med YAML
weight: 41
---

## Hvordan:
Kotlin har ikke innebygget støtte for YAML-tolkning og serialisering, men du kan bruke populære tredjepartsbiblioteker som `snakeyaml` (for generell YAML-tolkning) og `kotlinx.serialization` (med en YAML-formatutvidelse) for å jobbe med YAML-filer.

### Bruke `snakeyaml`
**Avhengighet:**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**Lese YAML:**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// Eksempel på bruk
fun main() {
    readYaml("config.yaml")
}
```
**Eksempel `config.yaml`:**
```yaml
database:
  host: localhost
  port: 5432
```
**Eksempel på utskrift:**
```
{database={host=localhost, port=5432}}
```

### Bruke `kotlinx.serialization` med YAML
Først, sørg for at du har biblioteket `kotlinx-serialization` med et passende YAML-støttebibliotek (hvis tilgjengelig, ettersom `kotlinx.serialization` hovedsakelig retter seg mot JSON og andre formater direkte).

**Avhengighet:**
```kotlin
// For JSON (illustrativ, sjekk for YAML-støtte eller alternative biblioteker)
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**Definere en serialiserbar dataklasse:**
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

Dessverre, på tidspunktet for skriving, kan direkte YAML-støtte i `kotlinx.serialization` være begrenset eller i utvikling. Du kan trenge å bruke en mellomliggende fremstilling (som å konvertere YAML til JSON med `snakeyaml` og deretter tolke JSON med `kotlinx.serialization`) eller se etter samfunnsdrevne YAML-serialiseringsprosjekter som er kompatible med `kotlinx.serialization`.

For JSON ville koden se slik ut:
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

Ettersom Kotlin og dets økosystem fortsetter å utvikle seg, hold et øye med den offisielle dokumentasjonen og samfunnsressurser for det siste innen YAML-støtte og biblioteker.
