---
aliases:
- /es/kotlin/handling-errors/
date: 2024-01-26 00:54:30.500222-07:00
description: "El manejo de errores es c\xF3mo tu c\xF3digo se ocupa de los problemas\
  \ que surgen durante la ejecuci\xF3n\u2014como manejar una bola curva sin dejarla\
  \ caer. Los\u2026"
lastmod: 2024-02-18 23:09:09.943989
model: gpt-4-1106-preview
summary: "El manejo de errores es c\xF3mo tu c\xF3digo se ocupa de los problemas que\
  \ surgen durante la ejecuci\xF3n\u2014como manejar una bola curva sin dejarla caer.\
  \ Los\u2026"
title: Manejo de errores
---

{{< edit_this_page >}}

## ¿Qué y por qué?
El manejo de errores es cómo tu código se ocupa de los problemas que surgen durante la ejecución—como manejar una bola curva sin dejarla caer. Los programadores lo hacen para prevenir fallos y ofrecer a los usuarios una experiencia fluida.

## Cómo hacerlo:
Kotlin proporciona `try`, `catch`, `finally` y `throw` para gestionar errores. Así es cómo los utilizas:

```Kotlin
fun main() {
    val numerador = 10
    val denominador = 0

    try {
        val resultado = numerador / denominador
        println("Resultado: $resultado")
    } catch (e: ArithmeticException) {
        println("No se puede dividir por cero, amigo.")
    } finally {
        println("Esto sucede pase lo que pase.")
    }
}
```

Salida:
```
No se puede dividir por cero, amigo.
Esto sucede pase lo que pase.
```

Si algo sale mal en el bloque `try`, la ejecución salta al `catch`. Captura el error específico lanzado (`ArithmeticException` en este caso). El bloque `finally` se ejecuta después, sin importar el resultado.

## Análisis Detallado
El bloque `try-catch` ha existido desde los primeros días de la programación—es como una red de seguridad. Kotlin también ofrece `throw` para lanzar manualmente una excepción al ruedo, y está `finally` para código que tiene que ejecutarse—trabajo de limpieza, a menudo.

Las alternativas incluyen el tipo `Result` y el `try` de Kotlin como una expresión.

```Kotlin
val result: Result<Int> = try {
    Result.success(numerador / denominador)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
Este enfoque devuelve un objeto `Result`—consigues un éxito o un fracaso sin el drama de una excepción no manejada.

La implementación en Kotlin es ordenada porque puedes usar `try` como una expresión, lo que significa que devuelve un valor. Opciones como estas hacen que el manejo de errores en Kotlin sea bastante versátil. Se trata de elegir la herramienta adecuada para el trabajo, como lo harías en un taller.

## Ver También
- Documentación de Kotlin sobre Excepciones: [Manejo de Excepciones en Kotlin](https://kotlinlang.org/docs/exception-handling.html)
- Documentación del tipo `Result` de Kotlin: [Resultado en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- Effective Java, 3ª Edición, de Joshua Bloch—grandes perspectivas sobre las excepciones, aunque es específico de Java.
