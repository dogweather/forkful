---
title:                "Trabajando con json"
html_title:           "Gleam: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/working-with-json.md"
---

{{< edit_this_page >}}

¡Hola programadores! ¿Alguna vez te has preguntado cómo procesar y almacenar datos de manera eficiente en tus aplicaciones? Bueno, ¡te tenemos cubierto! En este artículo, vamos a hablar sobre cómo trabajar con JSON en Gleam y por qué los programadores lo hacen. Así que ¡presta atención y prepárate para mejorar tus habilidades de programación!

## ¿Qué y por qué?

Trabajar con JSON (JavaScript Object Notation) implica utilizar un formato de texto sencillo para almacenar y transmitir datos. Es ampliamente utilizado en aplicaciones web y móviles para intercambiar información entre sistemas diferentes. Los programadores a menudo optan por usar JSON debido a su simplicidad y su capacidad para trabajar con diferentes lenguajes de programación.

## Cómo hacerlo

Ahora, veamos algunos ejemplos de cómo trabajar con JSON en Gleam. ¡Presta atención porque esto es realmente emocionante!

Primero, importamos el módulo de JSON en nuestra aplicación:

```
import gleam/json
```

Luego, podemos convertir un objeto de Gleam en una cadena JSON utilizando la función `encode`:

```
let my_object = {
  usuario: "Juan",
  edad: 25
}

let json_string = gleam/json.encode(my_object)
```

Esto producirá una cadena JSON similar a esta:

```
{"user":"Juan","age":25}
```

También podemos decodificar una cadena JSON en un objeto de Gleam utilizando la función `decode`:

```
let json_string = "{\"usuario\":\"Juan\",\"edad\":25}"
let object = gleam/json.decode(json_string)
```

Esto nos devolverá un objeto de Gleam con las mismas propiedades que nuestro objeto inicial.

## Profundizando

JSON fue originalmente desarrollado en la década de 1990 por Douglas Crockford. Desde entonces, ha sido ampliamente adoptado en la industria y se ha convertido en un estándar para el intercambio de datos en la web. Alternativamente, XML (Extensible Markup Language) es otro formato popular para el almacenamiento y transmisión de datos, pero se considera más complejo que JSON.

En términos de implementación, Gleam utiliza la biblioteca Rust `serde` para trabajar con JSON, lo que permite un rendimiento rápido y una alta compatibilidad con otros lenguajes de programación.

## Consulta también

Si estás interesado en aprender más sobre JSON en Gleam, aquí tienes algunos recursos útiles:

- [Documentación de Gleam sobre JSON](https://gleam.run/modules/json.html)
- [Sitio web oficial de JSON](https://www.json.org/json-es.html)
- [Tutorial de JSON en Gleam](https://learnbyexample.github.io/learn_gleam/json_gleam/)