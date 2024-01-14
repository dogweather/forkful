---
title:                "Haskell: Imprimiendo salida de depuración"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

A veces, cuando estamos programando en Haskell, nos encontramos con errores que son difíciles de entender. Es en estos momentos cuando es de gran ayuda imprimir la salida de depuración en nuestra consola. Esto nos permite ver el estado y los valores de nuestras variables en diferentes puntos de nuestro código, lo que nos ayuda a entender mejor lo que está sucediendo en nuestro programa.

## Cómo hacerlo

Para imprimir la salida de depuración en Haskell, podemos usar la función `Debug.Trace.trace` que se encuentra en el módulo `Debug.Trace`. Esta función toma dos argumentos: una cadena de texto y el valor que queremos imprimir. Por ejemplo, si tenemos una función que suma dos números y queremos imprimir la suma de los mismos, podemos hacer lo siguiente:

```Haskell
import Debug.Trace

sumar :: Int -> Int -> Int
sumar x y = x + y

main = do
  let resultado = sumar 2 3
  trace "El resultado es:" resultado
```

Al ejecutar este código, veremos en nuestra consola la siguiente salida:

```
El resultado es: 5
```

## Profundizando

Una vez que estemos más familiarizados con la función `Debug.Trace.trace`, podemos explorar otras opciones disponibles en el módulo `Debug.Trace` que nos permiten imprimir información de depuración más detallada, como el tiempo de ejecución de una función o la pila de llamadas.

También es importante tener en cuenta que imprimir demasiada salida de depuración puede ser contraproducente y afectar el rendimiento de nuestro programa. Por lo tanto, es importante utilizar esta herramienta con moderación y eliminar cualquier salida de depuración innecesaria antes de implementar nuestro código en producción.

## Ver también
- [Documentación oficial de Debug.Trace](https://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html)
- [Tutorial sobre depuración en Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-debugger)
- [Artículo sobre buenas prácticas de depuración en Haskell] (https://www.parsonsmatt.org/2018/05/19/how_to_debug_haskell_code.html)