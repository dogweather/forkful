---
title:                "Concatenación de cadenas de texto"
date:                  2024-01-20T17:34:54.568869-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenación de cadenas de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Concatenar cadenas es juntar dos o más textos en uno solo. Lo hacemos para construir mensajes, trabajar con datos y manipular texto dinámicamente en nuestros programas.

## Cómo hacerlo:
En Gleam, puedes concatenar cadenas con el operador `++`. Aquí tienes un ejemplo sencillo:

```gleam
pub fn main() {
  let saludo = "Hola"
  let mundo = "mundo!"
  let mensaje = saludo ++ " " ++ mundo
  mensaje
}
```

Salida:

```
"Hola mundo!"
```

## En Profundidad:
La concatenación de cadenas es un concepto antiguo en la programática, existente desde los inicios de los lenguajes de programación. Aunque concatenar es bastante sencillo y directo en Gleam, es importante considerar la eficiencia. Concatenar cadenas repetidamente puede ser costoso en términos de rendimiento, ya que crea nuevos objetos de cadena cada vez. En Gleam, como en muchos lenguajes funcionales, las cadenas son inmutables, lo que significa que no puedes cambiarlas, sólo crear nuevas.

Para situaciones más complejas, se pueden utilizar funciones de biblioteca para unir listas de cadenas o buffers más eficientes que no requieren crear nuevas cadenas todo el tiempo. Esto es algo que se considera en aplicaciones de alto rendimiento donde la eficiencia es crítica.

## Ver También:
- [Elixir String Concatenation](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html#concatenation): Un lenguaje relacionado que inspiró a Gleam, puede proporcionar más contexto y ejemplos de operaciones de cadenas.
- [Gleam Programming Language](https://gleam.run): Página oficial de Gleam para recursos adicionales y aprendizaje.
