---
title:                "Kotlin: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Por qué trabajar con JSON en Kotlin

JSON (JavaScript Object Notation) es un formato de intercambio de datos muy popular en el desarrollo de aplicaciones web y móviles. Utilizarlo en tus proyectos de Kotlin te permitirá almacenar, transmitir y manipular datos de manera eficiente y sencilla. En este post, aprenderás cómo trabajar con JSON en Kotlin y cómo aprovechar al máximo sus capacidades.

## Cómo trabajar con JSON en Kotlin

Para trabajar con JSON en Kotlin, primero debes importar la librería `kotlinx.serialization`. Esta librería te permitirá serializar y deserializar objetos en formato JSON. Para comenzar, crea una clase con los atributos que deseas convertir a JSON:

```Kotlin
class Persona(val nombre: String, val edad: Int, val ciudad: String)
```

Luego, utiliza la función `Json.encodeToString()` para serializar una instancia de esta clase y obtener un string JSON:

```Kotlin
val persona = Persona("Ana", 25, "Madrid")
val json = Json.encodeToString(persona)
println(json) // {"nombre": "Ana", "edad": 25, "ciudad": "Madrid"}
```

Para deserializar un objeto JSON y crear una instancia de la clase, puedes utilizar la función `Json.decodeFromString()`:

```Kotlin
val json = """{"nombre": "Pedro", "edad": 30, "ciudad": "Barcelona"}"""
val persona = Json.decodeFromString<Persona>(json)
println(persona.nombre) // Pedro
println(persona.edad) // 30
println(persona.ciudad) // Barcelona
```

Además, puedes utilizar las anotaciones `@Serializable` y `@SerialName` en tu clase para personalizar el formato de la serialización. Por ejemplo:

```Kotlin
@Serializable
class Producto(
    @SerialName("nombre") val nombre: String,
    @SerialName("precio") val precio: Double,
    @SerialName("descripcion") val descripcion: String
)
```

## Profundizando en el trabajo con JSON en Kotlin

Como has visto en el ejemplo anterior, trabajar con JSON en Kotlin es bastante sencillo gracias a la librería `kotlinx.serialization`. Sin embargo, esta librería también te permite manejar casos más complejos, como la serialización de objetos anidados, listas y mapas.

Además, si quieres trabajar con JSON de manera más eficiente y no utilizar la librería, puedes utilizar `GSON`, una librería desarrollada por Google para trabajar con JSON en Java y que también es compatible con Kotlin.

En resumen, el uso de JSON en Kotlin te permitirá manejar de manera eficiente tus datos y mejorar la comunicación entre tus aplicaciones y servicios web.

## Ver también

- [Documentación oficial de kotlinx.serialization](https://github.com/Kotlin/kotlinx.serialization)
- [Tutorial de Kotlin: Trabajar con JSON](https://kotlinlang.org/docs/tutorials/serialization.html)
- [Documentación oficial de GSON](https://github.com/google/gson)