---
title:                "Utilizando expresiones regulares"
html_title:           "Kotlin: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Las expresiones regulares son patrones de búsqueda que se utilizan para encontrar cadenas de texto específicas dentro de un texto más grande. Los programadores las utilizan para buscar y manipular datos de una manera más eficiente y precisa.

## Cómo:
```Kotlin
val texto = "¡Hola! Soy un texto de ejemplo."
val regex = Regex("[A-Za-z]+")

println(regex.find(texto)?.value) // Imprime "Hola"
println(regex.findAll(texto).toList()) // Imprime ["Hola", "Soy", "un", "texto", "de", "ejemplo"]
println(regex.replace(texto, "Adiós")) // Imprime "¡Adiós! Adiós un Adiós Adiós."
```

## Profundizando:
Las expresiones regulares tienen su origen en la teoría matemática y son una herramienta poderosa y versátil en programación. Existen alternativas como el uso de funciones de cadenas o el uso de bibliotecas externas, sin embargo, las expresiones regulares siguen siendo una opción popular debido a su eficiencia y flexibilidad. En Kotlin, se pueden utilizar a través de la clase `Regex` que permite buscar, reemplazar y manipular cadenas de texto.

## Ver también:
- [Documentación oficial de expresiones regulares en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)