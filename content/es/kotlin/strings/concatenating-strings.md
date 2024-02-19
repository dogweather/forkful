---
aliases:
- /es/kotlin/concatenating-strings/
date: 2024-01-20 17:35:08.322731-07:00
description: "La concatenaci\xF3n de cadenas es unir dos o m\xE1s strings para formar\
  \ uno nuevo. Los programadores la usan para construir mensajes, combinar datos y\
  \ generar\u2026"
lastmod: 2024-02-18 23:09:09.927213
model: gpt-4-1106-preview
summary: "La concatenaci\xF3n de cadenas es unir dos o m\xE1s strings para formar\
  \ uno nuevo. Los programadores la usan para construir mensajes, combinar datos y\
  \ generar\u2026"
title: "Concatenaci\xF3n de cadenas de texto"
---

{{< edit_this_page >}}

## Qué y Por Qué?

La concatenación de cadenas es unir dos o más strings para formar uno nuevo. Los programadores la usan para construir mensajes, combinar datos y generar salidas dinámicas.

## Cómo Hacerlo:

Concatenar con el operador `+`:

```Kotlin
val saludo = "Hola, "
val nombre = "Mundo"
val mensaje = saludo + nombre + "!"
println(mensaje) // Salida: Hola, Mundo!
```

Usando string templates (plantillas de cadenas):

```Kotlin
val edad = 30
val presentacion = "Tengo $edad años."
println(presentacion) // Salida: Tengo 30 años.
```

Con la función `concat()`:

```Kotlin
val str1 = "Kotlin "
val str2 = "es Genial."
val resultado = str1.concat(str2)
println(resultado) // Salida: Kotlin es Genial.
```

## Profundizando

Históricamente, la concatenación de strings ha sido una operación fundamental en la programación por su simplicidad y utilidad. 

Alternativas a la concatenación directa incluyen el uso de la clase `StringBuilder` para rendimiento en construcciones de strings complejas o la función `joinToString` para unir elementos de listas.

Detalles de implementación: en Kotlin, el operador `+` para strings está sobrecargado y finalmente hace uso de `StringBuilder` para una eficiencia óptima en la JVM.

## Ver También

- Documentación oficial de Kotlin sobre strings: [Strings - Kotlin Programming Language](https://kotlinlang.org/docs/strings.html)
- Kotlin API - StringBuilder: [StringBuilder - Kotlin Programming Language](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)
- Kotlin API - joinToString: [joinToString - Kotlin Programming Language](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/join-to-string.html)
