---
title:                "Interpolación de cadenas de texto"
date:                  2024-01-20T17:50:44.578302-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolación de cadenas de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Interpolar una cadena consiste en insertar variables o expresiones dentro de una cadena de texto. Los programadores lo hacen para construir cadenas de manera dinámica y eficiente, facilitando la manipulación de datos y la interacción con el usuario.

## Cómo:

En Gleam, usamos la función `string.concat` o la interpolación directa usando `#{}` para combinar textos y variables. Ejemplos:

```gleam
// Concatenación simple usando `string.concat`
let nombre = "Mundo"
let saludo = string.concat(["Hola, ", nombre, "!"])
// saludo es "Hola, Mundo!"

// Interpolación usando #{}
let precio = 9.99
let mensaje = "El total a pagar es: #{precio} euros."
// mensaje es "El total a pagar es: 9.99 euros."
```

## Profundización:

Históricamente, la interpolación de cadenas proviene de lenguajes como Perl y se ha propagado a muchos otros, incluidos los modernos como Ruby o JavaScript. En Gleam, que es un lenguaje funcional fuertemente tipado, la interpolación es más segura en cuanto a tipos, evitando algunos errores comunes en otros lenguajes. Alternativas a la interpolación pueden ser la concatenación manual (menos legible) o el uso de plantillas de texto externas (más complejas). Detalle de implementación: la interpolación en Gleam es eficiente y garantiza que el valor interpolado siempre será una cadena o será convertido a una de forma segura.

## Ver También:

- Comparación de métodos de interpolación en diferentes lenguajes: [String Interpolation in Programming Languages](https://en.wikipedia.org/wiki/String_interpolation#In_programming_languages)