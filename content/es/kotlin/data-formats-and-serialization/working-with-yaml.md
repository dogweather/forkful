---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:52.883729-07:00
description: "YAML, que significa \"YAML Ain't Markup Language\" (YAML no es un lenguaje\
  \ de marcado), es un formato de serializaci\xF3n de datos altamente legible que\
  \ se\u2026"
lastmod: '2024-03-13T22:44:59.056665-06:00'
model: gpt-4-0125-preview
summary: "YAML, que significa \"YAML Ain't Markup Language\" (YAML no es un lenguaje\
  \ de marcado), es un formato de serializaci\xF3n de datos altamente legible que\
  \ se utiliza a menudo para archivos de configuraci\xF3n, almacenamiento de datos\
  \ y mensajer\xEDa entre procesos."
title: Trabajando con YAML
weight: 41
---

## Cómo hacerlo:
Kotlin no tiene soporte integrado para el análisis y serialización de YAML, pero puedes utilizar bibliotecas de terceros populares como `snakeyaml` (para análisis de YAML en general) y `kotlinx.serialization` (con una extensión de formato YAML) para trabajar con archivos YAML.

### Usando `snakeyaml`
**Dependencia:**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**Leer YAML:**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// Ejemplo de uso
fun main() {
    readYaml("config.yaml")
}
```
**Ejemplo de `config.yaml`:**
```yaml
database:
  host: localhost
  port: 5432
```
**Salida de ejemplo:**
```
{database={host=localhost, port=5432}}
```

### Usando `kotlinx.serialization` con YAML
Primero, asegúrate de tener la biblioteca `kotlinx-serialization` con una biblioteca de soporte YAML adecuada (si está disponible, ya que `kotlinx.serialization` se dirige principalmente a JSON y otros formatos directamente).

**Dependencia:**
```kotlin
// Para JSON (ilustrativo, verifica el soporte de YAML o bibliotecas alternativas)
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**Define una clase de datos serializable:**
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

Desafortunadamente, en el momento de escribir esto, el soporte directo de YAML en `kotlinx.serialization` puede ser limitado o estar evolucionando. Es posible que necesites usar una representación intermedia (como convertir YAML a JSON con `snakeyaml` y luego analizar JSON con `kotlinx.serialization`) o buscar proyectos de serialización YAML impulsados por la comunidad compatibles con `kotlinx.serialization`.

Para JSON, el código se vería algo así:
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

A medida que Kotlin y su ecosistema continúan evolucionando, mantén un ojo en la documentación oficial y los recursos de la comunidad para lo último en soporte y bibliotecas YAML.
