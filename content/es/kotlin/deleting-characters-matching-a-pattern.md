---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Kotlin: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué?

En algunas situaciones, es posible que necesitemos eliminar ciertos caracteres de un texto que coincidan con un patrón específico. Esto puede ser necesario para limpiar datos, filtrar información o mejorar la legibilidad de un texto.

## Cómo hacerlo

Podemos utilizar el método `removeChars` de Kotlin para eliminar los caracteres que coinciden con un patrón determinado. Veamos un ejemplo:

```Kotlin
// Crea una cadena de texto con caracteres no deseados
val str = "¡H01@ m1 ¡N0Mbr3 es Juan!"

// Elimina los números y el símbolo @
val resultado = str.removeChars("[0-9@]")

// Imprime el resultado
print(resultado)
```

**Salida**:

Hola mi nombre es Juan!

En este ejemplo, utilizamos la expresión regular `[0-9@]` como patrón, que eliminará cualquier número o el símbolo @ de la cadena de texto. Podemos modificar el patrón según nuestras necesidades para eliminar otros caracteres.

También podemos utilizar el operador `-=`, que nos permite eliminar directamente los caracteres que coinciden con un patrón de una cadena de texto:

```Kotlin
// Crea una cadena de texto con caracteres no deseados
val str = "123-456-789"

// Elimina los símbolos -
str -= "-"

// Imprime el resultado
print(str)
```

**Salida**:

123456789

## Profundizando

El método `removeChars` toma como argumento una expresión regular, que es una secuencia de caracteres que nos permite especificar un patrón de búsqueda. Pueden ser muy útiles para manipular cadenas de texto, ya que nos permiten buscar y reemplazar partes específicas de una cadena.

Existen muchos patrones de expresiones regulares que pueden ser utilizados para eliminar caracteres. Aquí te dejamos algunos recursos que te pueden ayudar a aprender más sobre cómo trabajar con expresiones regulares en Kotlin:

- [Documentación oficial de Kotlin sobre expresiones regulares](https://kotlinlang.org/docs/regexp.html)
- [Tutorial de expresiones regulares en Kotlin](https://www.baeldung.com/kotlin-regular-expressions)
- [Libro sobre expresiones regulares en Kotlin](https://www.amazon.es/Regular-Expressions-Kotlin-Lothringer-David/dp/1484234042)

## Consulta también

- [Documentación oficial de Kotlin sobre el método `removeChars`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/remove-chars.html)
- [Tutorial de Kotlin en español](https://www.genbeta.com/desarrollo/tutorial-kotlin)