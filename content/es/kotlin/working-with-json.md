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

## ¿Qué es y por qué se utiliza JSON?

JSON (JavaScript Object Notation) es un formato ligero de intercambio de datos que se utiliza ampliamente en la programación para almacenar y transmitir información. Los programadores usan JSON porque es fácil de leer y escribir, y es compatible con muchos lenguajes de programación y plataformas.

## ¡Cómo hacerlo!

`Kotlin` proporciona una manera fácil de trabajar con JSON a través de la clase `JSONObject`. Podemos crear un objeto JSON utilizando su constructor y agregar pares clave-valor utilizando el operador `put`:

```Kotlin
val json = JSONObject()
json.put("nombre", "María")
json.put("edad", 25)
```

También podemos crear objetos JSON a partir de cadenas de texto utilizando el método `JSONObject()` y acceder a sus elementos utilizando el operador `get`:

```Kotlin
val userJson = JSONObject("{\"nombre\":\"Juan\",\"edad\":20}")
val nombre = userJson.get("nombre")  // Juan
val edad = userJson.get("edad")      // 20
```

Para convertir un objeto JSON a una cadena de texto, podemos usar el método `toString()`:

```Kotlin
val jsonString = json.toString() // {"nombre":"María","edad":25}
```

## Inmersión profunda

JSON fue creado originalmente por Douglas Crockford en 2001, como una alternativa más ligera al formato XML. Ha ganado gran popularidad en la programación web y en el intercambio de datos entre aplicaciones. Algunas alternativas a JSON son YAML, CSV y XML.

La implementación de JSON en Kotlin se basa en la biblioteca `org.json` del paquete `Java` y proporciona una API sencilla y eficiente para trabajar con objetos JSON.

## Ver también

- [JSON.org](https://www.json.org/json-es.html)
- [Introducción a JSON en programación (video en español)](https://www.youtube.com/watch?v=Lxmy7Ww3ndo)