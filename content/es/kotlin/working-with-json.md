---
title:                "Trabajando con JSON"
date:                  2024-01-19
simple_title:         "Trabajando con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Qué es & Por qué?
Trabajar con JSON implica manipular una estructura de datos ligera y formativa para el intercambio de datos. Los programadores lo utilizan porque es fácil de leer y escribir para humanos, y simple de parsear y generar para máquinas.

## Cómo hacerlo:
En Kotlin, puedes usar la biblioteca `kotlinx.serialization` para trabajar con JSON. Aquí tienes un ejemplo:

```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Serializando un objeto a JSON
    val json = Json.encodeToString(User("Carlos", 29))
    println(json) // Output: {"name":"Carlos","age":29}

    // Deserializando JSON a un objeto
    val jsonString = """{"name":"Ana", "age":34}"""
    val user = Json.decodeFromString<User>(jsonString)
    println(user) // Output: User(name=Ana, age=34)
}
```

## Análisis Profundo:
JSON (JavaScript Object Notation) surgió como una alternativa a XML, más sencilla y ligera. Mientras que otras alternativas, como YAML o TOML, también existen para la configuración y serialización de datos, JSON destaca por su compatibilidad universal en APIs web y su eficiencia en términos de rendimiento. Kotlin ofrece una integración moderna y potente con JSON a través de `kotlinx.serialization`, la cual se centra en la seguridad de los tipos y la interoperabilidad con el lenguaje.

## Ver También:
- Documentación oficial de `kotlinx.serialization`: [https://kotlinlang.org/docs/serialization.html](https://kotlinlang.org/docs/serialization.html)
- Guía de JSON: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
