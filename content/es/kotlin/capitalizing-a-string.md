---
title:                "Capitalización de una cadena"
html_title:           "Kotlin: Capitalización de una cadena"
simple_title:         "Capitalización de una cadena"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Capitalizar una cadena de texto se refiere a hacer que la primera letra de cada palabra en la cadena sea una letra mayúscula. Los programadores lo hacen para mejorar la legibilidad y presentación de textos en sus programas.

## Cómo hacerlo:
```Kotlin
val texto = "hola, ¿cómo estás?"
val textoCapitalizado = texto.capitalize()

println(textoCapitalizado)
```

Output:
Hola, ¿cómo estás?

## Profundizando:
Existen diferentes formas de capitalizar una cadena en Kotlin dependiendo de lo que se quiere lograr. Algunas alternativas incluyen `toLowerCase()`, `toUpperCase()`, `replaceFirstChar()`, entre otras. Además, es importante tener en cuenta el idioma y las reglas de capitalización de cada lenguaje para obtener los resultados deseados.

## Ver también:
- [Kotlin Doc - Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [The Ultimate Guide to Kotlin Strings](https://medium.com/swlh/the-ultimate-guide-to-kotlin-strings-361bb338788a)
- [Formato de texto: mayúsculas, minúsculas y capitalización en Kotlin](https://medium.com/@amaljugale/apps4all-tutorial-de-kotlin-6-formato-de-texto-may%C3%BAsculas-min%C3%BAsculas-y-capitalizaci%C3%B3n-8534d581698c)