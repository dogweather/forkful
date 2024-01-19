---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

# Imprimir Información de Depuración en Haskell

## ¿Qué & Por Qué?
Una salida de depuración es información de diagnóstico impresa por un programa para entender su comportamiento. Los programadores usan esta técnica para identificar y resolver problemas en su código más eficientemente.

## Cómo Hacerlo:

La función `print` en Haskell permite imprimir la salida de depuración. Aquí hay un ejemplo simple:

```Haskell
main :: IO ()
main = do 
   print "Hello, Debugging!"
```

El comando anterior imprimirá en la consola: `"Hello, Debugging!"`

Para imprimir el estado interno de una instancia, podemos usar la función `show`.

```Haskell
data Persona = Persona { nombre :: String, edad :: Int }

main = print $ Persona "Juan" 30
```

Este comando imprimirá en la consola: `Persona {nombre = "Juan", edad = 30}`

## Inmersión Profunda:

La función `print` tiene una larga historia en Haskell, ya que es parte fundamental del sistema IO característico de este lenguaje. Sin embargo, existen alternativas como las funciones `trace` y `traceShow` del módulo `Debug.Trace`, que permiten imprimir datos para depuración sin romper la pureza de una función.

```Haskell
import Debug.Trace
sum' :: [Int] -> Int
sum' xs = sum'' xs 0
  where
    sum'' []     t = traceShow t t
    sum'' (x:xs) t = sum'' xs (x+t)
```

El código anterior imprimirá el total acumulado en cada llamada recursiva.

Los métodos `print`, `trace`, y `traceShow` son implementados utilizando la mónada IO. Esta es una característica única de Haskell que le permite manejar operaciones con efectos secundarios como los IO en un marco seguro y predecible.

## Ver También:

Para profundizar en la depuración en Haskell, lee las siguientes fuentes:

1. Learn You a Haskell for Great Good: Una guía introductoria gratuita a Haskell. [Link](http://learnyouahaskell.com/)
2. Real World Haskell: Un libro de texto más avanzado en funciones IO y depuración. [Link](http://book.realworldhaskell.org/)
3. Documentación de Debug.Trace para más funciones de traza y detalles. [Link](https://hackage.haskell.org/package/base-4.14.1.0/docs/Debug-Trace.html)