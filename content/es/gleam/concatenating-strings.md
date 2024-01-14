---
title:                "Gleam: Uniendo cadenas de texto"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué
Si estás leyendo esto, probablemente tengas curiosidad sobre cómo concatenar cadenas en Gleam. Esta habilidad es importante para cualquier programador, ya que te permite combinar múltiples cadenas para crear una sola, lo cual es especialmente útil cuando estás trabajando con datos.

## Cómo hacerlo
```Gleam 
let nombre = "Juan"
let apellido = "García"

let nombre_completo = nombre <> " " <> apellido

IO.println(nombre_completo)
```

```
Juan García
```

La concatenación de cadenas en Gleam se realiza mediante el operador `<>`, el cual une dos cadenas en una sola. También puedes usarlo para unir más de dos cadenas al mismo tiempo. Solo recuerda colocar espacios en blanco entre cada cadena para que la salida sea legible.

## Profundizando
La concatenación de cadenas en Gleam se puede realizar no solo con cadenas de texto, sino también con otros tipos de datos como enteros y booleanos. Esto se logra gracias a la capacidad de Gleam de convertir automáticamente otros tipos de datos a cadenas antes de concatenarlos.

Además, el operador `<>` también funciona con variables, lo que significa que puedes unir cadenas almacenadas en variables en lugar de escribir todo en una sola línea de código.

## Ver también
- [Documentación oficial de Gleam sobre cadenas](https://gleam.run/book/tour/strings.html)
- [Ejemplos de cómo concatenar cadenas en Gleam](https://github.com/gleam-lang/gleam/blob/master/src/tests/strings_test.test)

¡Ahora que sabes cómo concatenar cadenas en Gleam, puedes seguir aprendiendo sobre otras funcionalidades del lenguaje!