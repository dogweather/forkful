---
title:                "Uniendo cadenas de texto"
html_title:           "Gleam: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

¡Hola programadores! Hoy hablaremos sobre una de las tareas más comunes en la programación: concatenar cadenas de texto. ¿Qué es eso y por qué los programadores lo hacemos? A continuación, exploraremos cómo hacerlo en Gleam, algunos detalles interesantes y enlaces útiles para seguir aprendiendo.

## ¿Qué y por qué?

Concatenar cadenas de texto significa simplemente unir o combinar varias cadenas de texto en una sola. Por ejemplo, si tenemos las cadenas "Hola" y "mundo",al concatenarlas obtendríamos "Hola mundo". ¿Por qué lo hacemos? Esto nos permite crear cadenas de texto más complejas y personalizadas para mostrar en nuestras aplicaciones. 

## ¡Cómo hacerlo!

Veamos un ejemplo en Gleam:

```
let hola = "Hola"
let mundo = "mundo"

let saludos = hola <> " " <> mundo

IO.println(saludos)

```
**Salida**: Hola mundo

En este código, primero definimos dos variables, "hola" y "mundo", con las cadenas de texto que queremos concatenar. Luego, utilizamos el operador `<>` para unir ambas cadenas con un espacio en blanco en medio. Finalmente, imprimimos el resultado con la función `IO.println()`.

También podemos concatenar más de dos cadenas, simplemente añadiendo más `<>` entre ellas:

```
let bienvenida = "Bienvenido"
let nombre = "Mateo"
let mensaje = bienvenida <> ", " <> nombre <> "!"

IO.println(mensaje)

```
**Salida**: Bienvenido, Mateo!

## Profundizando

### Contexto histórico

La concatenación de cadenas de texto ha sido una práctica común en la programación desde los inicios de la informática. Anteriormente, se hacía de manera manual utilizando funciones para unir caracteres individuales. Sin embargo, con la evolución de los lenguajes de programación, ahora hay operadores y funciones más eficientes para este propósito.

### Alternativas

Además del operador `<>` en Gleam, existen otras formas de concatenar cadenas, como la función `concat()` en Python y el operador `+` en Java. Cada lenguaje tiene su propia sintaxis, pero todos logran el mismo resultado.

### Detalles de implementación

En Gleam, cuando se utiliza el operador `<>`, se crea una nueva cadena de texto que contiene los caracteres de las cadenas originales. Por lo tanto, es importante tener en cuenta el tamaño de las cadenas que estamos concatenando, ya que puede afectar la eficiencia del programa.

## Ver también

- [Gleam Docs](https://gleam.run/documentation/) - la documentación oficial de Gleam, con más información sobre concatenar cadenas y otros aspectos del lenguaje.
- [Tutorial de Gleam](https://danielamatias.github.io/gleam-book/introduction.html) - un tutorial interactivo para aprender más sobre Gleam y sus características.
- [Concatenación de cadenas en otros lenguajes](https://www.learnpython.org/en/String_Formatting) - un artículo comparativo sobre cómo concatenar cadenas en varios lenguajes de programación.