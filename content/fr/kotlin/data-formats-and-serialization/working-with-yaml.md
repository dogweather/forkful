---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:47.316715-07:00
description: "Comment faire : Kotlin n'a pas de support int\xE9gr\xE9 pour l'analyse\
  \ et la s\xE9rialisation YAML, mais vous pouvez utiliser des biblioth\xE8ques tierces\
  \ populaires\u2026"
lastmod: '2024-03-13T22:44:57.762897-06:00'
model: gpt-4-0125-preview
summary: "Kotlin n'a pas de support int\xE9gr\xE9 pour l'analyse et la s\xE9rialisation\
  \ YAML, mais vous pouvez utiliser des biblioth\xE8ques tierces populaires telles\
  \ que `snakeyaml` (pour l'analyse YAML g\xE9n\xE9rale) et `kotlinx.serialization`\
  \ (avec une extension de format YAML) pour travailler avec des fichiers YAML."
title: Travailler avec YAML
weight: 41
---

## Comment faire :
Kotlin n'a pas de support intégré pour l'analyse et la sérialisation YAML, mais vous pouvez utiliser des bibliothèques tierces populaires telles que `snakeyaml` (pour l'analyse YAML générale) et `kotlinx.serialization` (avec une extension de format YAML) pour travailler avec des fichiers YAML.

### Utiliser `snakeyaml`
**Dépendance :**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**Lire YAML :**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// Exemple d'utilisation
fun main() {
    readYaml("config.yaml")
}
```
**Exemple de `config.yaml` :**
```yaml
database:
  host: localhost
  port: 5432
```
**Sortie d'exemple :**
```
{database={host=localhost, port=5432}}
```

### Utiliser `kotlinx.serialization` avec YAML
D'abord, assurez-vous d'avoir la bibliothèque `kotlinx-serialization` avec une bibliothèque de support YAML appropriée (si disponible, car `kotlinx.serialization` cible principalement JSON et d'autres formats directement).

**Dépendance :**
```kotlin
// Pour JSON (illustratif, vérifiez le support YAML ou les bibliothèques alternatives)
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**Définir une classe de données sérialisable :**
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

Malheureusement, au moment de la rédaction, le support YAML direct dans `kotlinx.serialization` peut être limité ou en évolution. Vous pourriez avoir besoin d'utiliser une représentation intermédiaire (comme convertir YAML en JSON avec `snakeyaml` et ensuite analyser JSON avec `kotlinx.serialization`) ou rechercher des projets de sérialisation YAML pilotés par la communauté compatibles avec `kotlinx.serialization`.

Pour JSON, le code ressemblerait à ceci :
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

À mesure que Kotlin et son écosystème continuent d'évoluer, restez à l'affût de la documentation officielle et des ressources communautaires pour les dernières informations sur le support YAML et les bibliothèques.
