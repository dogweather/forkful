---
title:    "Haskell: Imprimiendo salida de depuración"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por qué

Cuando estamos programando en Haskell, es importante que nuestro código funcione correctamente y sin errores. Una forma de asegurarnos de esto es imprimir mensajes de depuración durante la ejecución de nuestro programa. Esto nos permite ver el estado de nuestras variables y entender mejor cómo está funcionando nuestro código. En esta publicación, exploraremos por qué es importante imprimir mensajes de depuración en Haskell.

## Cómo hacerlo

Para imprimir mensajes de depuración en Haskell, utilizamos la función "trace" de la biblioteca Debug.Trace. Esta función toma dos argumentos: una cadena de texto y un valor. La cadena de texto se imprime junto con el valor en la consola de salida. Veamos un ejemplo:

```Haskell
import Debug.Trace

factorial :: Int -> Int
factorial n = trace ("Calculando el factorial de " ++ show n) $
    if n == 0 then 1
    else n * factorial (n-1)

main = do
    putStrLn "Ingresa un número:"
    n <- getLine
    let resultado = factorial (read n :: Int)
    putStrLn ("El resultado es: " ++ show resultado)
```

La salida de este programa sería la siguiente si ingresamos 5 como valor:

```
Ingresa un número:
5
Calculando el factorial de 5
Calculando el factorial de 4
Calculando el factorial de 3
Calculando el factorial de 2
Calculando el factorial de 1
El resultado es: 120
```

Podemos ver cómo se imprime cada mensaje de depuración seguido del valor correspondiente. Esto nos permite entender mejor cómo se está realizando el cálculo del factorial y asegurarnos de que no haya errores.

## Profundizando

Si queremos imprimir mensajes de depuración más complejos en Haskell, podemos utilizar la función "Debug.Trace.traceShow" en lugar de "Debug.Trace.trace". Esta función toma un valor de cualquier tipo y lo imprime utilizando la función "show". Esto es útil cuando queremos imprimir estructuras de datos más complejas.

Además, es importante tener en cuenta que al utilizar mensajes de depuración, nuestro programa puede volverse más lento debido al costo de imprimir en la consola. Por lo tanto, es importante utilizarlos solo cuando sea necesario y eliminarlos antes de la implementación final de nuestro programa.

## Ver también

- [Tutorial de Haskell](https://www.tutorialspoint.com/haskell/index.htm)
- [Documentación de Debug.Trace](https://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html)
- [Pautas de depuración en Haskell](https://wiki.haskell.org/Debugging)