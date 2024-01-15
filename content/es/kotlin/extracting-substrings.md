---
title:                "Extracción de subcadenas"
html_title:           "Kotlin: Extracción de subcadenas"
simple_title:         "Extracción de subcadenas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Si eres un programador principiante o avanzado, es muy probable que en algún momento necesites trabajar con cadenas de texto. Ya sea para manipular o validar datos, las subcadenas son una herramienta esencial en la programación. Por lo tanto, es importante conocer cómo extraer subcadenas en Kotlin para realizar tareas más eficientemente.

## Cómo

Para extraer una subcadena en Kotlin, se utiliza el método `substring()` que se encuentra en la clase `String`. Este método toma dos parámetros: el índice inicial y el índice final de la subcadena que se desea extraer.

```
Kotlin
val texto = "¡Hola, mi nombre es Juan!"
val subcadena = texto.substring(7, 17)
println(subcadena)

Salida:
mi nombre es
```

En el ejemplo anterior, se crea una variable `texto` con una oración y luego se extrae la subcadena que va desde el índice 7 (la letra "m") hasta el índice 17 (la letra "s"). La salida muestra la subcadena seleccionada.

Además de estos dos parámetros, el método `substring()` también puede tomar un solo parámetro, que es el índice inicial. En este caso, se extrae una subcadena que va desde el índice indicado hasta el final de la cadena original.

```
Kotlin
val texto = "¡Hola, mi nombre es Juan!"
val subcadena = texto.substring(7)
println(subcadena)

Salida:
mi nombre es Juan!
```

También es posible utilizar números negativos como índices, lo que indica que la subcadena se extraerá desde el final de la cadena original. Por ejemplo, si se utiliza `-4` como índice final, se extraerían los últimos 4 caracteres de la cadena.

```
Kotlin
val texto = "¡Hola, mi nombre es Juan!"
val subcadena = texto.substring(11, -1)
println(subcadena)

Salida:
me es Juan  ¡Hola, mi nomb
```

Es importante tener en cuenta que el primer índice siempre debe ser menor que el segundo, de lo contrario, se obtendrá una excepción `StringIndexOutOfBoundsException`.

## Deep Dive

Además del método `substring()`, Kotlin también cuenta con el método `slice()`, que permite extraer subcadenas a partir de una lista de índices. Este método toma como parámetro una lista de índices y devuelve una subcadena que incluye los caracteres en dichos índices.

```
Kotlin
val texto = "amigos"
val indices = listOf(1, 3, 5)
val subcadena = texto.slice(indices)
println(subcadena)

Salida:
mgs
```

También existe el método `subSequence()`, que funciona de manera similar al `substring()`, pero devuelve una subsecuencia de la cadena original en lugar de una nueva cadena. Esta subsecuencia se puede utilizar para tomar elementos de una colección.

```
Kotlin
val texto = "Kotlin es genial"
val subsecuencia = texto.subSequence(0, 6)
println(subsecuencia)

Salida:
Kotlin
```

## Ver también

Si quieres saber más sobre la manipulación de cadenas de texto en Kotlin, puedes consultar estos recursos:

- [Documentación oficial de Kotlin sobre `substring()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [Tutorial de Kotlin de Codecademy sobre cadenas de texto](https://www.codecademy.com/courses/introduction-to-kotlin/lessons/kotlin-strings/exercises/kotlin-strings-basics)
- [Tutorial de W3Schools sobre el búcle `for` en Kotlin](https://www.w3schools.com/kotlin/kotlin_for_loop.asp)