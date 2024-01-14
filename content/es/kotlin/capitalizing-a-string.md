---
title:                "Kotlin: Majusculeando una cadena"
simple_title:         "Majusculeando una cadena"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

A menudo en la programación, es necesario capitalizar una cadena de texto, es decir, convertir la primera letra de cada palabra en mayúscula. Esto puede ser necesario para cumplir con ciertos estándares de formato o para mejorar la legibilidad de los datos. En este artículo, aprenderemos cómo capitalizar una cadena en el lenguaje de programación Kotlin.

## Cómo hacerlo

Para capitalizar una cadena en Kotlin, podemos utilizar la función `capitalize()` que viene incorporada en la clase `String`. Esta función convierte la primera letra de la cadena en mayúscula y deja el resto de la cadena sin cambios. Veamos un ejemplo de cómo usarla:

```Kotlin
val palabra = "hola"
println(palabra.capitalize())

// Output: Hola
```

En el código anterior, declaramos una variable llamada `palabra` y le asignamos el valor de "hola". Luego, utilizamos la función `capitalize()` para convertir la primera letra de la cadena en mayúscula y mostramos el resultado por consola.

En caso de que queramos capitalizar todas las palabras de una cadena, podemos utilizar la función `capitalizeWords()` de la libreriía `kotlin.text`. Veamos un ejemplo:

```Kotlin
import kotlin.text.capitalizeWords

val frase = "hola mundo"
println(frase.capitalizeWords())

// Output: Hola Mundo
```

En este caso, además de importar la función `capitalizeWords()`, utilizamos la anotación `import` para importar toda la librería `kotlin.text`. Esto nos permite utilizar diferentes funciones relacionadas con el manejo de cadenas de texto.

## Deep Dive

En Kotlin, la función `capitalize()` y `capitalizeWords()` son implementadas internamente utilizando el método `toUpperCase()` de Java. Este método toma una letra y la convierte en mayúscula según las reglas de la tabla de conversión de Unicode. Por lo tanto, si queremos capitalizar una cadena en español, por ejemplo, podemos asegurarnos de que se apliquen las reglas correctas importando la clase `java.util.Locale` y pasando el idioma como parámetro a la función `capitalizeWords()`.

```Kotlin
import java.util.Locale
import kotlin.text.capitalizeWords

val frase = "hola mundo"
println(frase.capitalizeWords(Locale("es")))

// Output: Hola Mundo
```

Con este pequeño ajuste, podemos asegurarnos de que nuestra cadena se capitalice correctamente según las reglas del idioma que necesitemos.

## Ver también

- [Documentación oficial de Kotlin sobre la función `capitalize()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Documentación oficial de Kotlin sobre la función `capitalizeWords()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/index.html)
- [Respuesta de StackOverflow sobre cómo capitalizar una cadena en Kotlin](https://stackoverflow.com/questions/43793420/capitalizing-strings-in-kotlin)