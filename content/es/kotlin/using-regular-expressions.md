---
title:                "Kotlin: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué utilizar expresiones regulares en Kotlin

Las expresiones regulares son una herramienta poderosa para realizar búsquedas y manipulación de texto en cualquier lenguaje de programación, incluyendo Kotlin. Con ellas, puedes encontrar y extraer patrones específicos de una cadena de texto de manera eficiente y precisa.

## Cómo utilizar expresiones regulares en Kotlin

Primero, debes importar la librería de expresiones regulares de Kotlin ```kotlin.text.Regex```. Luego, puedes utilizarla para crear un objeto de tipo ```Regex``` con el patrón que deseas buscar en la cadena de texto. Por ejemplo: 

```Kotlin
val regex = Regex("[0-9]+")
val text = "¡Hola! Mi número de teléfono es 123456789."
val matches = regex.findAll(text)
```

En este caso, utilizamos la expresión regular ```[0-9]+``` para buscar cualquier secuencia de números en la cadena de texto. Luego, usamos el método ```findAll()``` para obtener una lista de todas las coincidencias encontradas. Puedes acceder a la lista de coincidencias y utilizarlas como mejor te convenga, por ejemplo, imprimirlas en la consola.

## Profundizando en el uso de expresiones regulares

Las expresiones regulares en Kotlin ofrecen muchas funcionalidades y opciones avanzadas que puedes utilizar para hacer búsquedas más específicas. Puedes utilizar caracteres especiales como ```*``` (cero o más repeticiones), ```+``` (una o más repeticiones), ```?``` (cero o una repetición), entre otros. También puedes utilizar ```[]``` para especificar un grupo de caracteres permitidos para la búsqueda.

Además, puedes utilizar la sintaxis ```(?:...)``` para crear grupos de captura que te permitirá acceder a ciertas partes de la coincidencia encontrada. Otra característica útil es ```|```, que se utiliza para hacer búsquedas alternativas. Puedes encontrar más información y ejemplos en la documentación oficial de expresiones regulares de Kotlin.

## Ver también

- [Documentación oficial de expresiones regulares de Kotlin](https://kotlinlang.org/docs/regexp.html)
- [Tutorial de expresiones regulares en español](https://www.w3schools.com/python/python_regex.asp)
- [Ejemplos prácticos de uso de expresiones regulares en Kotlin](https://blog.kotlin-academy.com/regular-expressions-in-kotlin-85956aeef33d)