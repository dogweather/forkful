---
title:                "Trabajando con json"
html_title:           "Kotlin: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con JSON en Kotlin?

Si estás desarrollando en Kotlin, es muy probable que en algún momento tengas que trabajar con estructuras de datos en formato JSON. Este formato es ampliamente utilizado para el intercambio de información entre aplicaciones, por lo que conocer cómo manejarlo en Kotlin es una habilidad esencial para cualquier desarrollador.

## Cómo trabajar con JSON en Kotlin

Para trabajar con JSON en Kotlin, necesitaremos importar la librería "kotlinx.serialization", la cual nos proporciona las herramientas necesarias para serializar y deserializar objetos a formato JSON.
```Kotlin
// Importamos la librería
import kotlinx.serialization.*
import kotlinx.serialization.json.Json
```

A continuación, definiremos una clase que representará un objeto en formato JSON. En este caso, utilizaremos una clase llamada "Persona" con las propiedades "nombre" y "edad".
```Kotlin
// Definimos la clase Persona
@Serializable
data class Persona(
    val nombre: String,
    val edad: Int
)
```

Para serializar una instancia de la clase Persona a formato JSON, utilizaremos la función "stringify" de la librería "kotlinx.serialization", pasándole como parámetro una instancia de la clase y especificando el formato de salida.
```Kotlin
// Creamos una instancia de la clase Persona
val persona = Persona("Juan", 25)

// Serializamos la instancia a formato JSON
val json = Json.stringify(Pesona.serializer(), persona)

// Imprimimos el resultado
println(json)
```

La salida de este código sería:
```Kotlin
{"nombre":"Juan","edad":25}
```

Para deserializar un objeto JSON a una instancia de la clase Persona, utilizaremos la función "parse" de la librería "kotlinx.serialization", pasándole como parámetro el formato de entrada y el objeto JSON.
```Kotlin
// Definimos un objeto JSON
val json = """{"nombre":"María","edad":30}"""

// Deserializamos el objeto a una instancia de la clase Persona
val persona = Json.parse(Persona.serializer(), json)

// Imprimimos el resultado
println(persona.nombre)
println(persona.edad)
```

La salida de este código sería:
```Kotlin
María
30
```

## Profundizando en el manejo de JSON en Kotlin

Además de serializar y deserializar objetos a formato JSON, la librería "kotlinx.serialization" nos permite realizar operaciones más avanzadas, como la manipulación de objetos anidados, la personalización del proceso de serialización y deserialización, entre otras. Puedes consultar la documentación oficial para obtener más información y ejemplos.

## Ver también

- Documentación oficial de "kotlinx.serialization": https://github.com/Kotlin/kotlinx.serialization
- Tutorial de JSON en Kotlin: https://www.baeldung.com/kotlin-json
- Ejemplos de uso de "kotlinx.serialization": https://ktor.io/docs/serialization.html