---
date: 2024-01-26 04:14:42.696727-07:00
description: "C\xF3mo hacerlo: Para iniciar el GHCi (entorno interactivo del compilador\
  \ de Haskell de Glasgow), simplemente escribe `ghci` en tu terminal. Aqu\xED te\u2026"
lastmod: '2024-03-13T22:44:59.120499-06:00'
model: gpt-4-0125-preview
summary: Para iniciar el GHCi (entorno interactivo del compilador de Haskell de Glasgow),
  simplemente escribe `ghci` en tu terminal.
title: Usando una shell interactiva (REPL)
weight: 34
---

## Cómo hacerlo:
Para iniciar el GHCi (entorno interactivo del compilador de Haskell de Glasgow), simplemente escribe `ghci` en tu terminal. Aquí te mostramos cómo usarlo:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

La muestra de salida explica que `x` es una variable numérica y muestra que duplicarla resulta en 10.

## Profundización:
El GHCi de Haskell ha avanzado mucho desde su creación. Ofrece un conjunto rico de características como la autocompletación de tabulaciones, entrada de múltiples líneas y carga de paquetes. Alternativas como Hugs son mayormente históricas ahora, siendo GHCi el estándar. GHCi compila código en tiempo justo cada vez que ingresas una expresión, dándote una forma eficiente de probar tu código Haskell.

## Ver También:
- [La Guía del Usuario de GHC – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [¡Aprende Haskell por el Bien Mayor! – Comenzando](http://learnyouahaskell.com/starting-out#hello-world)
- [Wiki de Haskell – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
