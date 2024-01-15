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

## ¿Por qué trabajar con JSON?

Si estás buscando una forma sencilla y eficiente de intercambiar datos entre diferentes aplicaciones o sistemas, JSON es tu mejor opción. Con su sintaxis ligera y su fácil integración en muchos lenguajes de programación, JSON se ha convertido en un formato de datos muy popular en la actualidad.

## Cómo hacerlo

```Gleam
// Ejemplo de cómo crear un objeto JSON
let persona = {
  nombre: "Maria",
  edad: 25,
  trabajo: "Desarrollador"
}

// Ejemplo de cómo acceder a un valor en el objeto JSON
let nombre = persona.nombre // Maria

// Ejemplo de cómo convertir un objeto JSON en una cadena
let json = gleam_json.encode(persona) // {"nombre":"Maria","edad":25,"trabajo":"Desarrollador"}
```

Ahora que sabes cómo crear y acceder a los valores de un objeto JSON, es importante tener en cuenta algunas cosas a la hora de trabajar con este formato de datos.

## Profundizando en JSON

Algunos datos importantes para recordar sobre JSON incluyen:

- Todos los valores de JSON deben estar entre comillas dobles.
- Los nombres de las propiedades también deben estar entre comillas dobles.
- Los valores pueden ser de cualquier tipo de dato válido, incluyendo string, number, boolean, null, array y object.
- Los objetos JSON pueden tener múltiples propiedades separadas por comas.
- Una de las ventajas de JSON es que es fácil de analizar y generar en la mayoría de los lenguajes de programación, incluyendo Gleam.

## Vea también

- [Documentación oficial de JSON](https://www.json.org/json-es.html)
- [Introducción a JSON en Gleam](https://gleam.run/book/tour/json.html)
- [Librería JSON para Gleam](https://github.com/lpil/gleam_json)