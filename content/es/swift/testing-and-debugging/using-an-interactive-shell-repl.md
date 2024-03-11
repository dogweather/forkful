---
date: 2024-01-26 04:17:49.340442-07:00
description: "Usar una shell interactiva, o un Bucle de Leer-Evaluar-Imprimir (REPL,\
  \ por sus siglas en ingl\xE9s), te permite programar de manera interactiva. Los\u2026"
lastmod: '2024-03-11T00:14:33.244355-06:00'
model: gpt-4-0125-preview
summary: "Usar una shell interactiva, o un Bucle de Leer-Evaluar-Imprimir (REPL, por\
  \ sus siglas en ingl\xE9s), te permite programar de manera interactiva. Los\u2026"
title: Usando una shell interactiva (REPL)
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Usar una shell interactiva, o un Bucle de Leer-Evaluar-Imprimir (REPL, por sus siglas en inglés), te permite programar de manera interactiva. Los programadores lo utilizan para probar fragmentos de Swift rápidamente, depurar o aprender el lenguaje.

## Cómo hacerlo:
Invoca el REPL abriendo un terminal y ejecutando `swift`. Escribe código directamente y presiona Enter para ejecutarlo. Aquí tienes un adelanto:

```Swift
1> let greeting = "¡Hola, REPL!"
greeting: String = "¡Hola, REPL!"
2> print(greeting)
¡Hola, REPL!
```

Sal con `:quit` o `Control-D`.

## Profundización
Las raíces del REPL se remontan a los intérpretes de Lisp en los años '60. El REPL de Swift se asienta sobre LLVM, un poderoso marco de compilación, ofreciendo más que solo interpretación básica: es una herramienta completa con autocompletado, depuración y más. REPL es excelente para aprender o prototipar, pero no es un entorno de desarrollo independiente. Algunas personas prefieren usar Playgrounds en Xcode para un enfoque más gráfico y basado en archivos, mientras que otras se mantienen con la edición y ejecución tradicional de scripts.

Debajo del capó, el REPL de Swift compila dinámicamente el código a lenguaje de máquina y lo ejecuta, lo que es relativamente rápido. También puede acceder a cualquier módulo de Swift compilado, o incluso a bibliotecas de C, lo que lo hace bastante poderoso. Sin embargo, ten en cuenta que no todo funciona perfectamente en REPL; algunas características de Swift, especialmente aquellas que requieren configuraciones de proyecto complejas o archivos de storyboard, no funcionarán aquí.

## Ver también
- [Swift.org - Empezando](https://www.swift.org/getting-started/#using-the-repl)
- Introducción a Playgrounds de Xcode de Apple [Introducción a los Playgrounds de Xcode](https://developer.apple.com/videos/play/wwdc2014/408/)
- [Proyecto LLVM](https://llvm.org/)
