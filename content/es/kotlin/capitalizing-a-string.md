---
title:                "Capitalizando una cadena"
html_title:           "Kotlin: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué capitalizar una cadena

Capitalizar una cadena es un proceso común en la programación en el que se convierte la primera letra de cada palabra en mayúscula. Esto puede ser útil para mejorar la legibilidad de una cadena o para fines de formato. En este artículo, aprenderemos cómo capitalizar una cadena en Kotlin y profundizaremos en cómo funciona este proceso.

## Cómo hacerlo

Para capitalizar una cadena en Kotlin, podemos simplemente usar el método `capitalize()` en la cadena deseada. Por ejemplo:

```Kotlin
val texto = "hola, ¿cómo estás?"
println(texto.capitalize())
```

Este código producirá la siguiente salida:

```
Hola, ¿cómo estás?
```

El método `capitalize()` también funciona con cadenas que contienen varias palabras separadas por espacios o caracteres especiales. También se puede usar en cadenas que contienen caracteres Unicode.

Otra opción para capitalizar una cadena es usar el método `replaceFirstChar()` en combinación con el método `uppercaseChar()` para convertir la primera letra en mayúscula. Por ejemplo:

```Kotlin
val texto = "capitalización es divertida"
println(texto.replaceFirstChar { it.uppercaseChar() })
```

Este código producirá la siguiente salida:

```
Capitalización es divertida
```

## Profundizando en la capitalización de cadenas

El método `capitalize()` internamente utiliza el método `replaceFirstChar()` mencionado anteriormente para convertir la primera letra de la cadena en mayúscula. Sin embargo, también realiza algunas comprobaciones adicionales para asegurarse de que la cadena no sea nula antes de realizar la capitalización. Si la cadena es nula, el método devuelve simplemente `null`.

Además, el proceso de capitalizar también tiene en cuenta la configuración regional del dispositivo en el que se ejecuta el código. Esto significa que, dependiendo del idioma configurado, algunas letras pueden convertirse en mayúsculas de manera diferente.

En resumen, capitalizar una cadena en Kotlin es un proceso sencillo que se puede realizar fácilmente con uno de los métodos mencionados anteriormente. Sin embargo, es importante tener en cuenta que el resultado final puede variar dependiendo de las circunstancias.

## Ver también

- [Método capitalize en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Método replaceFirstChar en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace-first-char.html)