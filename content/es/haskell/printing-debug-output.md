---
title:    "Haskell: Imprimiendo salida de depuración"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces, cuando estamos programando en Haskell, nos encontramos con errores o comportamientos inesperados en nuestro código. Una forma muy útil de solucionar estos problemas es utilizando la técnica de imprimir salida de depuración. Esto nos permite ver el valor de variables en diferentes puntos de nuestro programa y entender cómo está funcionando realmente nuestro código.

## Cómo hacerlo

Para imprimir salida de depuración en Haskell, simplemente utilizamos la función `print` y le pasamos como argumento la variable que queremos ver. Por ejemplo:

```Haskell
nombre = "Juan"
print nombre
```

Este código imprimirá "Juan" en la consola, permitiéndonos ver el valor de la variable `nombre` en ese momento. Además, podemos utilizar la función `show` para imprimir valores de tipos más complejos, como listas o tuplas. Por ejemplo:

```Haskell
numeros = [1, 2, 3]
print (show numeros)
```

Este código imprimirá "[1,2,3]" en la consola, permitiéndonos ver el contenido de la lista `numeros`.

Otra forma de imprimir salida de depuración es utilizando `putStrLn`, que nos permite imprimir cualquier tipo de datos en la consola. Por ejemplo:

```Haskell
edad = 25
putStrLn ("Tengo " ++ show edad ++ " años")
```

Este código imprimirá "Tengo 25 años" en la consola. Podemos utilizar esta función en cualquier parte de nuestro código para imprimir información útil de depuración.

## Profundizando

Además de imprimir valores de variables, también podemos utilizar la función `trace` del módulo `Debug.Trace` para imprimir mensajes de depuración en momentos específicos de nuestro código. Por ejemplo:

```Haskell
import Debug.Trace

multiplicar x y = trace "Multiplicando..." (x * y)

resultado = multiplicar 4 3
print resultado
```

Este código imprimirá "Multiplicando..." en la consola antes de realizar la multiplicación y luego imprimirá el resultado. Esto nos permite entender qué está ocurriendo en cada etapa de nuestro programa.

También podemos utilizar `traceShow` para imprimir tanto el mensaje de depuración como el valor de una variable. Por ejemplo:

```Haskell
nombre = "Ana"
traceShow ("El nombre es " ++ nombre) nombre
```

Este código imprimirá "El nombre es Ana" en la consola y también devolverá "Ana" como valor de retorno.

## Ver también

- [Documentación de Haskell sobre impresión de salida de depuración](https://www.haskell.org/tutorial/io.html#output)
- [Tutorial de depuración en Haskell](https://dev.to/nchiusano/haskell-debugging-crash-course-d4l)
- [Módulo Debug.Trace en Haskell](https://hackage.haskell.org/package/base/docs/Debug-Trace.html)