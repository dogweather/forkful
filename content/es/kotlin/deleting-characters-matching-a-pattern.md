---
title:                "Kotlin: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un desarrollador de Kotlin, es posible que en algún momento hayas necesitado eliminar caracteres de una cadena que coinciden con un patrón específico. Esto podría ser útil en situaciones como limpiar datos de entrada o formatear una cadena según ciertas reglas. En este artículo, discutiremos cómo realizar esta tarea de manera eficiente y efectiva en Kotlin.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Kotlin, podemos utilizar el método `replace()` en una cadena. Este método toma dos argumentos: el patrón de búsqueda y la cadena de reemplazo.

```
val cadena = "¡Kotlin es genial!"
val nuevaCadena = cadena.replace(Regex("o")) { "" }
println(nuevaCadena)
// Output: ¡Ktlin es genial!
```

En este ejemplo, utilizamos una expresión regular para definir el patrón de búsqueda, en este caso, la letra `o`. Luego, pasamos una función lambda como segundo argumento, que en este caso simplemente devuelve una cadena vacía `""`. Esto significa que cualquier coincidencia con la letra `o` será reemplazada por una cadena vacía, eliminándola de la cadena original.

También podemos utilizar el método `replace()` para reemplazar varias coincidencias en una sola llamada. Por ejemplo, si queremos eliminar todas las vocales de una cadena, podemos usar una expresión regular que contenga todas las vocales y reemplazarlas por una cadena vacía.

```
val cadena = "¡Kotlin es genial!"
val nuevaCadena = cadena.replace(Regex("[aeiou]")) { "" }
println(nuevaCadena)
// Output: ¡Ktln s gnl!
```

En este caso, hemos utilizado una clase de caracteres en la expresión regular, que buscará cualquier coincidencia con las vocales `a, e, i, o, u`.

## Profundizando

Si queremos ser más específicos con las coincidencias que queremos eliminar, podemos utilizar más opciones en nuestra expresión regular. Por ejemplo, podemos especificar una clase de caracteres negada para excluir ciertos caracteres, o utilizar cuantificadores para eliminar ciertos patrones repetitivos.

También podemos utilizar el método `replaceFirst()` si solo queremos eliminar la primera coincidencia, en lugar de todas las coincidencias en la cadena.

Por último, vale la pena mencionar que el método `replace()` también funciona con cadenas que no contienen el patrón de búsqueda. En ese caso, la cadena original se devolverá sin cambios.

## Ver también

- [Documentación de Kotlin sobre expresiones regulares](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/kotlin.-string/replace.html)
- [Kotlin Regex Class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Tutorial de expresiones regulares en Kotlin](https://kotlinlang.org/docs/regex.html)