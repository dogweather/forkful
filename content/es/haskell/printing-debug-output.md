---
title:                "Imprimiendo salida de depuración"
html_title:           "Haskell: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

# ¿Por qué imprimir mensajes de depuración en Haskell?

¿Alguna vez te has encontrado con errores inexplicables en tu código Haskell? ¿Te ha costado horas descubrir el origen de un bug? Imprimir mensajes de depuración puede ser una herramienta útil para solucionar estos problemas de manera más eficiente.

# Cómo hacerlo

En Haskell, podemos imprimir mensajes de depuración utilizando la función `trace` de la biblioteca `Debug.Trace`. Esta función toma un `String` como primer argumento y cualquier tipo de dato como segundo argumento. Por ejemplo:

```Haskell
import Debug.Trace

sumar :: Int -> Int -> Int
sumar x y = trace ("sumando " ++ show x ++ " y " ++ show y) (x + y)

main = do
  let resultado = sumar 5 7
  print resultado
```
Salida:
```
sumando 5 y 7
12
```
Podemos ver que la función `trace` imprime el mensaje de depuración en la consola antes de devolver el resultado de la operación.

# Un poco más profundo

La función `trace` es útil para imprimir mensajes de depuración en una sola línea de código, pero ¿qué pasa si necesitamos imprimir varios mensajes en diferentes partes del código? Para ello, podemos utilizar la función `traceShow` que nos permite imprimir un valor junto a su tipo. Por ejemplo:

```Haskell
import Debug.Trace

multiplicar :: Int -> Int -> Int
multiplicar x y =
  traceShow x $
  traceShow y $
  x * y

main = do
  let resultado = multiplicar 2 7
  print resultado
```
Salida:
```
2
7
14
```
En este ejemplo, podemos ver que se imprimen los valores de `x` y `y` antes de realizar la multiplicación. Esta técnica puede ser especialmente útil para entender cómo se están utilizando los valores en una función.

# Ver también
- [Guía de depuración en Haskell](https://wiki.haskell.org/Debugging)
- [Documentación de `Debug.Trace`](https://hackage.haskell.org/package/base-4.15.1.0/docs/Debug-Trace.html)