---
title:                "Kotlin: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas?

Al programar en Kotlin, a menudo necesitamos realizar operaciones en cadenas de texto. Una de ellas es la conversión a minúsculas, lo que es útil para realizar búsquedas o comparaciones de texto sin importar mayúsculas o minúsculas. En este artículo aprenderemos cómo convertir una cadena a minúsculas en Kotlin.

## Cómo hacerlo

La conversión a minúsculas en Kotlin es sencilla y se puede hacer de varias formas. A continuación, se presentan dos ejemplos utilizando dos métodos diferentes:

```Kotlin
// Ejemplo 1: utilizando el método toLowerCase()
val cadena = "Hola Mundo"
val cadenaMinusculas = cadena.toLowerCase()
println(cadenaMinusculas)
// Salida: hola mundo
```

```Kotlin
// Ejemplo 2: utilizando la función de extensión de Kotlin
val frase = "Convertir ESTA cadena A MINÚSCULAS"
val fraseMinusculas = frase.toLowerCase()
println(fraseMinusculas)
// Salida: convertir esta cadena a minúsculas
```

En el primer ejemplo, utilizamos el método `toLowerCase()` que pertenece a la clase `String` y devuelve una nueva cadena en minúsculas. En el segundo ejemplo, utilizamos la función de extensión `toLowerCase()` de Kotlin, que también devuelve una nueva cadena en minúsculas.

## Profundizando

Además de los métodos y funciones de extensión mencionados anteriormente, hay otras formas de convertir una cadena a minúsculas en Kotlin. Por ejemplo, podemos utilizar regex o la biblioteca `java.util.Locale` para manejar caracteres especiales y emojis en la conversión.

Otra cosa importante a tener en cuenta es que la conversión a minúsculas es sensible al idioma. Esto significa que la salida puede variar dependiendo del idioma del dispositivo o de la región donde se ejecute el código.

En resumen, la conversión a minúsculas en Kotlin es una tarea común al trabajar con cadenas de texto y hay varias formas de hacerlo. Es importante elegir la opción adecuada según nuestras necesidades y considerar el idioma y las particularidades del texto a tratar.

## Ver también

- [Método toLowerCase() en la documentación oficial de Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Funciones de extensión en la documentación oficial de Kotlin](https://kotlinlang.org/docs/reference/extensions.html)
- [Ejemplos de expresiones regulares en Kotlin](https://www.baeldung.com/kotlin-regex)
- [Clase Locale en la documentación de Java](https://docs.oracle.com/javase/7/docs/api/java/util/Locale.html)