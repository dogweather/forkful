---
title:                "Haskell: Escribiendo en el error estándar"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir a la salida de error estándar en Haskell

Escribir a la salida de error estándar (STDERR) puede ser útil para mostrar mensajes de depuración o reportar errores en nuestros programas en Haskell. Esto nos permite tener un control más detallado y específico sobre la información que mostramos al usuario y puede ayudarnos a identificar y solucionar problemas más fácilmente.

## Cómo escribir a la salida de error estándar en Haskell

En Haskell, podemos imprimir mensajes a STDERR utilizando la función `hPutStrLn` del módulo `System.IO`. Esta función toma como argumento una cadena de texto y la imprime a la salida de error estándar. Por ejemplo:

```Haskell
import System.IO

main = do
    hPutStrLn stderr "¡Hola, mundo!"
    hPutStrLn stderr "Este es un mensaje de error."

-- Output:
-- ¡Hola, mundo!
-- Este es un mensaje de error.
```

Podemos utilizar esta función dentro de cualquier función `IO` y también podemos combinarla con otras funciones para formatear mensajes de error más complejos. Por ejemplo:

```Haskell
import System.IO

printError :: String -> IO ()
printError msg = do
    progName <- getProgName
    hPutStrLn stderr $ "[" ++ progName ++ "]: ERROR - " ++ msg

main = do
    printError "No se pudo conectar al servidor."
```

Este código imprimirá un mensaje de error en el formato "[nombre de programa]: ERROR - mensaje de error" a la salida de error estándar.

## Profundizando en la escritura a la salida de error estándar en Haskell

Además de la función `hPutStrLn`, también podemos utilizar otras funciones del módulo `System.IO` para manejar la salida de error estándar. Por ejemplo, `hPutStr` imprime una cadena sin agregar un salto de línea al final, mientras que `hPrint` puede imprimir cualquier tipo de dato que sea instancia de la clase `Show`.

También podemos utilizar la función `withFile` para abrir un archivo y escribir a él específicamente a la salida de error estándar. De esta forma, podemos separar los mensajes de depuración de los mensajes de error y escribirlos en un archivo separado.

## Ver también

- [Documentación de la función `hPutStrLn` en el módulo `System.IO`](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html#v:hPutStrLn)
- [Tutorial de Haskell en Español](https://www.monografias.com/trabajos97/haskell/haskell.shtml)
- [Ejemplos de código en Haskell](https://github.com/dbartondesign/learn-you-a-haskell-exercises)