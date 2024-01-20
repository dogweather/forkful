---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La búsqueda y reemplazo de texto se refiere a la localización de cadenas de texto específicas en el código y la sustitución de estás por otras. Los programadores lo hacen para modificar valores, nombres o corregir errores de manera rápida y eficiente.

## Como hacerlo:

En Kotlin, las funciones `replace()` y `replaceFirst()` nos permiten realizar estas tareas. Aquí te mostramos un ejemplo sencillo:
```Kotlin
fun main() {
    var texto = "Amo el café en la mañana y el café en la noche"
    println(texto.replace("café", "té"))
}
```
Salida:
```
Amo el té en la mañana y el té en la noche
```

Y si solo queremos reemplazar la primera coincidencia usamos `replaceFirst()` así:
```Kotlin
fun main() {
    var texto = "Amo el café en la mañana y el café en la noche"
    println(texto.replaceFirst("café", "té"))
}
```
Salida:
```
Amo el té en la mañana y el café en la noche
```

## Análisis más profundo

Históricamente, la búsqueda y reemplazo de texto ha sido una herramienta valiosa en la programación desde sus inicios. Las herramientas habituales son la búsqueda de expresiones regulares y los comandos de sustitución de texto.

En Kotlin, el método `replace()` es muy versátil, ya que pueden usarse expresiones regulares como argumentos para buscar y reemplazar contenido de manera más especifica.

Alternativamente podrías usar el método `substring()`, pero es más limitado y requiere más trabajo para obtener resultados similares. 

El `replace()` trabaja devolviendo una nueva cadena basada en la original, sustituyendo todas las coincidencias para `replace()` o la primera para `replaceFirst()`.

## Ver también

Para profundizar más en este tema puedes visitar estos enlaces:
- Documentación oficial de Kotlin [replace()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)