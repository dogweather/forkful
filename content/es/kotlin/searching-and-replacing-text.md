---
title:                "Kotlin: Buscando y reemplazando texto."
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué: 
En la programación, es común tener que realizar cambios masivos en el texto de un archivo o documento. La búsqueda y reemplazo de texto es una técnica útil para hacer estos cambios de manera rápida y eficiente. En lugar de tener que hacer cambios uno por uno, podemos utilizar esta técnica para hacer cambios en todo el texto al mismo tiempo.

## Cómo hacerlo:
Para realizar una búsqueda y reemplazo en Kotlin, podemos utilizar el operador `replace` en una cadena de texto. Por ejemplo, si queremos reemplazar la palabra "hola" por "adiós" en un texto, podemos hacerlo de la siguiente manera:

```Kotlin
val texto = "Hola, ¿cómo estás?"
val textoModificado = texto.replace("hola", "adiós")

println(textoModificado)
// Output: Adiós, ¿cómo estás?
```

En este ejemplo, la cadena `texto` es modificada utilizando el método `replace` y el resultado es almacenado en la variable `textoModificado`. Podemos ver que la palabra "hola" ha sido reemplazada por "adiós" en nuestro texto. 

También podemos realizar una búsqueda y reemplazo utilizando expresiones regulares. Por ejemplo, si queremos reemplazar todos los números en una cadena por letras, podemos hacerlo de la siguiente manera:

```Kotlin
val texto = "123,456,789"
val textoModificado = texto.replace("[0-9]+".toRegex(), "letra")

println(textoModificado)
// Output: letra,letra,letra
```

En este ejemplo, utilizamos la función `replace` con una expresión regular que encuentra patrones numéricos y los reemplaza por la palabra "letra". Esto nos permite realizar cambios en múltiples partes de un texto con una sola línea de código.

## Profundizando:
En Kotlin, también existen otras funciones y operadores que nos permiten realizar búsquedas y reemplazos más complejos. Algunos de ellos son `replaceFirst`, `replaceAfter`, `replaceBefore` y `replaceRange`. Estas funciones nos permiten especificar una posición o rango específico en el texto donde queremos realizar el reemplazo. También existen funciones para ignorar mayúsculas y minúsculas, así como también para utilizar expresiones regulares más avanzadas.

Otra forma de realizar una búsqueda y reemplazo es utilizando la función `regexReplace`. Esta función nos permite especificar una expresión regular y una función que se ejecutará cada vez que se encuentre un patrón. Esto nos da aún más control sobre el proceso de reemplazo.

## Ver también:
- Documentación oficial de Kotlin para la función [replace](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- Ejemplos de uso de expresiones regulares con [Kotlin](https://www.techiedelight.com/check-string-contains-number-using-regex-kotlin/)
- Otras funciones útiles para manipular cadenas de texto en Kotlin, como [substring](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html) y [split](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/split.html).