---
title:                "Uso de expresiones regulares"
date:                  2024-01-19
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qué son y por qué usarlos?
Las expresiones regulares (regex) permiten buscar patrones específicos en texto. Son herramientas potentes para la validación de formatos, la extracción de información, y la manipulación de cadenas de texto.

## Cómo hacerlo:
```kotlin
fun main() {
    // Definir un patrón de regex para números de teléfono
    val patron = "\\d{3}-\\d{3}-\\d{4}".toRegex()

    // Texto de ejemplo para la búsqueda
    val texto = "Mi número es 123-456-7890."

    // Buscar coincidencias
    val resultado = patron.find(texto)

    // Imprimir el resultado
    resultado?.let {
        println("Número encontrado: ${it.value}")
    }
}
```
Salida: 
```
Número encontrado: 123-456-7890
```

## Análisis Profundo:
Las expresiones regulares tienen su origen en los trabajos de teoría matemática del siglo XX. En Kotlin, y en muchos otros lenguajes, se implementan utilizando clases específicas que encapsulan tanto el patrón como las operaciones de búsqueda y manipulación. Hay alternativas como métodos de cadenas (split, replace) para operaciones simples, pero regex es superior en flexibilidad y potencia.

## Ver También:
- [Documentación oficial de Kotlin sobre Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/index.html)
- [Tutorial interactivo de Regex](https://regexone.com/)
- [Herramienta online para probar expresiones regulares](https://regex101.com/)
