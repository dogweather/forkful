---
title:                "Gleam: Convertir una cadena a minúsculas"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena de texto a minúsculas puede ser útil al manipular o comparar cadenas en un programa. Al convertir todas las letras a minúsculas, se evitan errores comunes de comparación de cadenas, como diferencias en mayúsculas y minúsculas.

## Cómo hacerlo

Para convertir una cadena de texto a minúsculas en Gleam, puedes utilizar la función `String.to_lower`. Por ejemplo:

```Gleam
let nombre = "Juan"
let nombre_minusculas = String.to_lower(nombre)
```
El resultado será una nueva cadena de texto con el valor "juan".

Otra forma de convertir una cadena a minúsculas es utilizando patrones de coincidencia de Gleam. Por ejemplo:

```Gleam
let nombre = "Ana"
let nombre_minusculas = case String.to_lower(nombre) of
  Ok(nuevo_nombre) -> nuevo_nombre
  Err(error) -> "Error al convertir la cadena"
```
En este caso, se utiliza una expresión `case` para manejar el posible resultado de la función `String.to_lower`.

## Profundizando

Gleam proporciona diferentes mecanismos para trabajar con cadenas de texto, como la función `split` para dividir cadenas y la función `join` para combinarlas. Una de las funciones más útiles para manipular cadenas es `map`, que permite aplicar funciones a cada carácter de una cadena.

Al utilizar `map` y la función `Char.to_lower`, es posible iterar sobre cada carácter de una cadena y convertirlo a minúsculas. Por ejemplo:

```Gleam
let nombre = "Pepe"
let nombre_minusculas = String.map(Char.to_lower, nombre)
```

El resultado final será una nueva cadena de texto con el valor "pepe".

## Ver también

- Documentación de Gleam sobre cadenas de texto: https://gleam.run/book/core/strings.html
- Tutorial de Gleam para principiantes: https://gleam.run/blog/getting-started-with-gleam.html