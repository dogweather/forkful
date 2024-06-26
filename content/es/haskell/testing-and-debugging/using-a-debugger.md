---
date: 2024-01-26 03:49:23.488550-07:00
description: "C\xF3mo hacerlo: Vamos a dar un paseo con GHCi, el entorno interactivo\
  \ de Haskell que puede actuar como un depurador b\xE1sico. Lo inicias con tu c\xF3\
  digo de\u2026"
lastmod: '2024-03-13T22:44:59.123300-06:00'
model: gpt-4-0125-preview
summary: "Vamos a dar un paseo con GHCi, el entorno interactivo de Haskell que puede\
  \ actuar como un depurador b\xE1sico."
title: Usando un depurador
weight: 35
---

## Cómo hacerlo:
Vamos a dar un paseo con GHCi, el entorno interactivo de Haskell que puede actuar como un depurador básico. Lo inicias con tu código de Haskell y comienzas a indagar. Aquí tienes un ejemplo:

```Haskell
main :: IO ()
main = do
    putStrLn "Hey, ¿cuál es tu nombre?"
    name <- getLine
    putStrLn $ "Hola, " ++ name ++ "! Vamos a depurar."
    let result = buggyFunction 5
    print result

buggyFunction :: Int -> Int
buggyFunction n = n * 2 -- Pretende que hay un error aquí
```

Para comenzar a depurar con GHCi:

```bash
$ ghci TuArchivoHaskell.hs
```

Establece un punto de interrupción en `buggyFunction`:

```Haskell
Prelude> :break buggyFunction
```

Ejecuta tu programa:

```Haskell
Prelude> :main
Hey, ¿cuál es tu nombre?
```

Tu programa se pausa en `buggyFunction`. Ahora puedes inspeccionar variables, avanzar paso a paso por el código y evaluar expresiones.

## Inmersión Profunda:
Históricamente, la reputación de Haskell por sus funciones puras y tipado fuerte llevó a la creencia de que las herramientas de depuración eran menos críticas. La realidad es diferente: los programas complejos siempre se benefician de buenas herramientas de depuración. GHCi proporciona comandos básicos de depuración. Sin embargo, para una experiencia más visual o aplicaciones a mayor escala, podrías explorar entornos de desarrollo integrado (IDEs) con depuradores integrados, como Visual Studio Code con extensiones de Haskell o el plugin de Haskell de IntelliJ.

Alternativas al depurador incluyen el uso de declaraciones de impresión, conocido como "depuración printf", o aprovechar el sistema de tipos fuertes de Haskell para hacer estados incorrectos no representables. Sin embargo, a veces no hay nada que reemplace avanzar paso a paso por el código.

En cuanto a los detalles de implementación, el depurador de Haskell trabaja con el sistema de tiempo de ejecución. Puede manejar puntos de interrupción, ejecución paso a paso y permitir la inspección de variables. Sin embargo, como Haskell se evalúa perezosamente, las cosas pueden volverse un poco no intuitivas. Depurar un programa Haskell a menudo significa mantener un ojo en cuándo y cómo se evalúan las expresiones.

## Ver También:
- [Guía del Usuario de GHC - Depurador](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [Plugin de Haskell en IntelliJ](https://plugins.jetbrains.com/plugin/8258-intellij-haskell)
