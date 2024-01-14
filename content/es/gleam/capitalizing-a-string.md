---
title:    "Gleam: Capitalizando un string"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por qué capitalizar una cadena en Gleam

La capitalización de cadenas es una tarea común en la programación, ya sea para presentar datos en un formato más legible o para fines de manipulación de datos. En Gleam, este proceso se puede realizar de manera sencilla y eficiente gracias a su sintaxis y funciones incorporadas.

## Cómo hacerlo

Para capitalizar una cadena en Gleam, simplemente se puede utilizar la función `String.capitalize()`. Por ejemplo, si queremos capitalizar la cadena "hola mundo", el código se vería así:

```Gleam
let saludo = "hola mundo"
let saludo_capitalizado = String.capitalize(saludo)
```

El resultado sería la cadena "Hola mundo". Sin embargo, también se puede utilizar la función `String.capitalize_words()` si se desea capitalizar cada palabra individualmente en una cadena. Por ejemplo:

```Gleam
let frase = "bienvenidos al mundo de la programación"
let frase_capitalizada = String.capitalize_words(frase)
```

El resultado sería la cadena "Bienvenidos Al Mundo De La Programación". ¡Muy útil para títulos de artículos o nombres de variables!

## Profundizando

Detrás de las funciones `String.capitalize()` y `String.capitalize_words()` se encuentra la función `String.map_chars()`. Esta función permite mapear una función a cada caracter en una cadena. Entonces, cuando utilizamos `String.capitalize()` o `String.capitalize_words()`, en realidad estamos aplicando una función anónima que capitaliza el primer caracter de cada palabra o cadena.

Este enfoque funcional es una de las características principales de Gleam y permite escribir código más expresivo y fácil de entender. Además, el proceso de capitalización es más eficiente que utilizar un bucle for o una expresión regular en otros lenguajes de programación.

## Ver también

- [Documentación de funciones de cadena en Gleam](https://gleam.run/core/String.html)
- [Tutorial de Gleam en español](https://gleam.run/assets/pdf/Gleam_es.pdf)
- [Artículo sobre programación funcional en Gleam](https://medium.com/swlh/functional-programming-in-gleam-dc5bbe74e5f4)