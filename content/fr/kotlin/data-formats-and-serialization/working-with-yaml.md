---
title:                "Travailler avec YAML"
date:                  2024-02-03T19:25:47.316715-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
YAML, qui signifie YAML Ain't Markup Language (YAML n'est pas un langage de balisage), est un format de sérialisation de données très lisible souvent utilisé pour les fichiers de configuration, le stockage de données et la messagerie inter-processus. Les programmeurs travaillent fréquemment avec YAML pour gérer les configurations et les paramètres de manière structurée mais simple, profitant de sa clarté et de sa simplicité par rapport à JSON ou XML lorsque la lisibilité est importante.

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
