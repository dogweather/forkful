---
aliases:
- /pl/kotlin/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:00.424325-07:00
description: "YAML, co rozszyfrowuje si\u0119 jako YAML Ain't Markup Language, to\
  \ wysoce czytelny format serializacji danych, cz\u0119sto u\u017Cywany do plik\xF3\
  w konfiguracyjnych,\u2026"
lastmod: 2024-02-18 23:08:49.589423
model: gpt-4-0125-preview
summary: "YAML, co rozszyfrowuje si\u0119 jako YAML Ain't Markup Language, to wysoce\
  \ czytelny format serializacji danych, cz\u0119sto u\u017Cywany do plik\xF3w konfiguracyjnych,\u2026"
title: Praca z YAML
---

{{< edit_this_page >}}

## Co i dlaczego?
YAML, co rozszyfrowuje się jako YAML Ain't Markup Language, to wysoce czytelny format serializacji danych, często używany do plików konfiguracyjnych, przechowywania danych oraz komunikacji międzyprocesowej. Programiści często pracują z YAML, aby zarządzać konfiguracjami i ustawieniami w sposób uporządkowany, a zarazem prosty, korzystając z jego przejrzystości i prostoty nad JSON lub XML, gdy ważna jest czytelność.

## Jak to zrobić:
Kotlin nie ma wbudowanego wsparcia dla parsowania i serializacji YAML, ale można wykorzystać popularne biblioteki stron trzecich, takie jak `snakeyaml` (do ogólnego parsowania YAML) oraz `kotlinx.serialization` (z rozszerzeniem formatu YAML) do pracy z plikami YAML.

### Korzystając z `snakeyaml`
**Zależność:**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**Odczyt YAML:**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// Przykładowe użycie
fun main() {
    readYaml("config.yaml")
}
```
**Przykładowy `config.yaml`:**
```yaml
database:
  host: localhost
  port: 5432
```
**Przykładowe wyjście:**
```
{database={host=localhost, port=5432}}
```
### Korzystając z `kotlinx.serialization` w połączeniu z YAML
Najpierw upewnij się, że masz bibliotekę `kotlinx-serialization` z odpowiednią biblioteką wsparcia YAML (jeśli dostępna, ponieważ `kotlinx.serialization` głównie kieruje się do JSON i innych formatów bezpośrednio).

**Zależność:**
```kotlin
// Dla JSON (ilustracyjnie, sprawdź wsparcie dla YAML lub alternatywne biblioteki)
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**Zdefiniuj serializowalną klasę danych:**
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

Niestety, w momencie pisania tego tekstu, bezpośrednie wsparcie YAML w `kotlinx.serialization` może być ograniczone lub ewoluować. Może być konieczne użycie pośredniej reprezentacji (takiej jak konwersja YAML na JSON za pomocą `snakeyaml` a następnie parsowanie JSON z `kotlinx.serialization`) lub poszukiwanie projektów serializacji YAML prowadzonych przez społeczność i kompatybilnych z `kotlinx.serialization`.

Dla JSON kod wyglądałby mniej więcej tak:
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

Jako że Kotlin i jego ekosystem ciągle się rozwijają, zwracaj uwagę na oficjalną dokumentację i zasoby społeczności, aby być na bieżąco z najnowszymi informacjami o wsparciu YAML i bibliotekach.
