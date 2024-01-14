---
title:                "Kotlin: Eliminando caracteres que coinciden con un patrón"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# ¿Por qué borrar caracteres que coinciden con un patrón?

A menudo, en la programación, necesitamos manejar cadenas de texto que contienen caracteres que no son deseados o no son relevantes para nuestro propósito. Por ejemplo, puede que necesitemos eliminar caracteres de espacio en blanco, espacios, guiones, etc. en una dirección o número de teléfono antes de almacenarlos en una base de datos o mostrarlos en un formulario. En estos casos, borrar caracteres que coinciden con un patrón puede ahorrarnos tiempo y esfuerzo.

## Cómo hacerlo

Para borrar caracteres que coinciden con un patrón en Kotlin, podemos utilizar el método `replace()` de la clase `String`. Este método toma dos parámetros: el primer parámetro es el patrón de caracteres que queremos eliminar y el segundo parámetro es el reemplazo que queremos utilizar en su lugar.

```Kotlin
val direccion = "123 Calle Principal"
val direccionLimpia = direccion.replace("\\s".toRegex(), "")
```

En el ejemplo anterior, utilizamos el patrón `\\s` para representar cualquier espacio en blanco y reemplazamos esos espacios con una cadena vacía `""`, lo que da como resultado `123CallePrincipal`.

Podemos utilizar múltiples `replace()` para borrar diferentes patrones de caracteres en una sola cadena. Además, también podemos utilizar estos métodos para reemplazar caracteres con otros diferentes o con caracteres especiales, como la `ñ` o `á`.

## Profundizando en el tema

Si queremos ser más específicos al borrar caracteres que coinciden con un patrón, podemos utilizar expresiones regulares en lugar de cadenas de texto simples en el primer parámetro del método `replace()`. Las expresiones regulares nos permiten definir patrones más complejos utilizando una sintaxis específica.

Por ejemplo, si queremos borrar todos los caracteres que no sean letras o números de una cadena de texto, podemos utilizar la expresión regular `[^A-Za-z0-9]`, que indica "todo lo que no sea una letra de la A a la Z, mayúscula o minúscula, o un número del 0 al 9".

```Kotlin
val numero = "(555) 123-4567"
val numeroLimpio = numero.replace("[^0-9]".toRegex(), "")
```

En este caso, utilizamos la expresión regular `[^0-9]` para representar todos los caracteres que no sean números y los reemplazamos con una cadena vacía, lo que dará como resultado `5551234567`.

## Ver también

- [Apuntes de expresiones regulares en Kotlin](https://kotlinlang.org/docs/regex.html)
- [Método `replace()` en la documentación de Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)