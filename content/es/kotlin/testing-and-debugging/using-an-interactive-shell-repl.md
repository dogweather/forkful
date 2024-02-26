---
date: 2024-01-26 04:15:34.725538-07:00
description: "Un REPL (Read-Eval-Print Loop, o Bucle de Leer-Evaluar-Imprimir) es\
  \ un entorno de programaci\xF3n de computadoras simple e interactivo. Los programadores\
  \ lo\u2026"
lastmod: '2024-02-25T18:49:55.509325-07:00'
model: gpt-4-0125-preview
summary: "Un REPL (Read-Eval-Print Loop, o Bucle de Leer-Evaluar-Imprimir) es un entorno\
  \ de programaci\xF3n de computadoras simple e interactivo. Los programadores lo\u2026"
title: Usando una shell interactiva (REPL)
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Un REPL (Read-Eval-Print Loop, o Bucle de Leer-Evaluar-Imprimir) es un entorno de programación de computadoras simple e interactivo. Los programadores lo usan para pruebas de codificación rápidas, probar fragmentos de código, o aprender la sintaxis de un lenguaje sin necesidad de crear una aplicación completa.

## Cómo hacerlo:
Iniciar el REPL de Kotlin es muy fácil. Abre tu terminal y escribe `kotlinc`. Aterrizarás en la shell de Kotlin. Intentemos definir una variable e imprimir su valor:

```kotlin
Bienvenido a Kotlin versión 1.7.10 (JRE 1.8.0_292-b10)
Escribe :help para ayuda, :quit para salir
>>> val saludo = "¡Hola, REPL de Kotlin!"
>>> println(saludo)
¡Hola, REPL de Kotlin!
```

## Profundizando
El REPL de Kotlin se estrenó con el lenguaje para fomentar la experimentación. Es similar al shell interactivo de Python, pero adaptado para la sintaxis y peculiaridades de Kotlin. ¿Alternativas? Entornos interactivos en IDEs, como IntelliJ IDEA, y espacios de juegos en línea de Kotlin. El REPL funciona compilando código al vuelo, proporcionando retroalimentación instantánea, crucial para el aprendizaje y la depuración.

## Ver también
- Documentación de Kotlin sobre REPL: [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- Prueba Kotlin en el navegador: [https://play.kotlinlang.org](https://play.kotlinlang.org)
- Plugin JetBrains Kotlin Playground para IntelliJ IDEA.
