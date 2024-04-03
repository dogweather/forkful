---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:51.400492-07:00
description: "Come fare: Kotlin non ha un supporto integrato per l'analisi e la serializzazione\
  \ di YAML, ma \xE8 possibile utilizzare popolari librerie di terze parti come\u2026"
lastmod: '2024-03-13T22:44:43.410971-06:00'
model: gpt-4-0125-preview
summary: "Kotlin non ha un supporto integrato per l'analisi e la serializzazione di\
  \ YAML, ma \xE8 possibile utilizzare popolari librerie di terze parti come `snakeyaml`\
  \ (per l'analisi generale di YAML) e `kotlinx.serialization` (con un'estensione\
  \ di formato YAML) per lavorare con file YAML."
title: Lavorare con YAML
weight: 41
---

## Come fare:
Kotlin non ha un supporto integrato per l'analisi e la serializzazione di YAML, ma è possibile utilizzare popolari librerie di terze parti come `snakeyaml` (per l'analisi generale di YAML) e `kotlinx.serialization` (con un'estensione di formato YAML) per lavorare con file YAML.

### Utilizzando `snakeyaml`
**Dipendenza:**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**Leggere YAML:**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// Utilizzo di esempio
fun main() {
    readYaml("config.yaml")
}
```
**Esempio `config.yaml`:**
```yaml
database:
  host: localhost
  port: 5432
```
**Output di esempio:**
```
{database={host=localhost, port=5432}}
```

### Utilizzando `kotlinx.serialization` con YAML
Per prima cosa, assicurati di avere la libreria `kotlinx-serialization` con una libreria di supporto YAML adatta (se disponibile, dato che `kotlinx.serialization` mira principalmente a JSON e altri formati direttamente).

**Dipendenza:**
```kotlin
// Per JSON (illustrativo, verifica il supporto YAML o le librerie alternative)
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**Definire una classe di dati serializzabile:**
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

Sfortunatamente, al momento della scrittura, il supporto diretto a YAML in `kotlinx.serialization` potrebbe essere limitato o in evoluzione. Potrebbe essere necessario utilizzare una rappresentazione intermedia (come convertire YAML in JSON con `snakeyaml` e poi analizzare JSON con `kotlinx.serialization`) o cercare progetti di serializzazione YAML guidati dalla comunità compatibili con `kotlinx.serialization`.

Per JSON, il codice sarebbe qualcosa di simile a questo:
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

Poiché Kotlin e il suo ecosistema continuano a evolversi, tieni d'occhio la documentazione ufficiale e le risorse della comunità per le ultime novità riguardo il supporto e le librerie YAML.
