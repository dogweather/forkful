---
title:                "Gleam: Buscar y reemplazar texto"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

El proceso de búsqueda y reemplazo de texto es una habilidad crucial en la programación que te permitirá automatizar tareas y ahorrar tiempo en proyectos de gran escala. Aprender a realizar estas operaciones correctamente te ayudará a mejorar la eficiencia de tu código y a trabajar de manera más rápida y precisa.

## Cómo hacerlo

Para realizar una búsqueda y reemplazo en Gleam, puedes utilizar la función `String.replace()` y pasarle como argumentos el texto a buscar y el texto a reemplazar. Por ejemplo:

```Gleam
let texto = "¡Bienvenido a mi blog sobre programación con Gleam!"
let nuevo_texto = texto.replace("programación", "desarrollo")
```
La variable `nuevo_texto` ahora contendrá la frase "¡Bienvenido a mi blog sobre desarrollo con Gleam!".

Puedes también utilizar expresiones regulares para realizar búsquedas más complejas y precisas. Por ejemplo, si quisieras reemplazar todas las letras mayúsculas en un texto por minúsculas, podrías hacerlo de la siguiente manera:

```Gleam
let texto = "MI NOMBRE ES JUAN."
let nuevo_texto = texto.replace([A-Z], \x -> String.to_lower([x]))
```
Ahora la variable `nuevo_texto` tendrá la frase "mi nombre es juan.".

Presta atención al uso de `\x -> String.to_lower([x])`, que es una función anónima que convierte cada letra mayúscula encontrada en la expresión regular a minúscula.


## Inmersión profunda

Ahora que ya sabes cómo realizar una búsqueda y reemplazo de texto básico en Gleam, es importante que conozcas algunas variantes y opciones disponibles. Por ejemplo, puedes utilizar el método `String.replace_first()` para reemplazar solo la primera aparición de un texto en una cadena.

Además, es importante mencionar que la función `String.replace()` en realidad devuelve una nueva cadena con el texto reemplazado, en lugar de modificar la cadena original. Esto es importante tenerlo en cuenta si quieres guardar los cambios en la cadena original.

Existen también otras funciones útiles para trabajar con cadenas en Gleam, como `String.contains()` para verificar si una cadena contiene un cierto texto, o `String.split()` para dividir una cadena en una lista a partir de un separador específico.

## Ver también

Para más información sobre el uso de expresiones regulares en Gleam, puedes consultar la [documentación oficial](https://gleam.run/articles/regular-expressions) del lenguaje.

Si quieres profundizar aún más en el poder de las cadenas y la manipulación de texto en Gleam, te recomendamos este [tutorial completo](https://gleam.run/articles/case-study-automated-gleam-compiler-releases) sobre la creación de un sistema automatizado de lanzamiento de compiladores con Gleam.

¡Esperamos que este artículo te haya sido útil en tu camino de aprendizaje en Gleam! ¡Sigue practicando y mejorando tus habilidades para convertirte en un experto en este lenguaje de programación funcional moderno y eficiente!