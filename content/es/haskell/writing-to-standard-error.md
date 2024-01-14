---
title:                "Haskell: Escribiendo a la entrada estándar"
simple_title:         "Escribiendo a la entrada estándar"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# ¿Por qué escribir a la salida estándar en Haskell?

Escribir a la salida estándar, o *standard error* en inglés, es una técnica muy útil en la programación en Haskell. Permite imprimir mensajes de error o información adicional al usuario durante la ejecución del programa. Además, puede ayudar en la depuración de errores y en la comprensión del funcionamiento interno del código.

## Cómo hacerlo

Para escribir a la salida estándar en Haskell, usamos la función `hPutStrLn` del módulo `System.IO`. Por ejemplo, si queremos imprimir un mensaje de error cuando una operación falla, podemos hacerlo de la siguiente manera:

```Haskell
import System.IO

main = do
    -- código de la operación
    catchIOError (error "La operación ha fallado") (\_ -> hPutStrLn stderr "Ha ocurrido un error")
```

En este ejemplo, usamos la función `catchIOError` para capturar la excepción generada por la operación y, en caso de fallar, imprimir un mensaje de error en la salida estándar usando `hPutStrLn`, que recibe como primer argumento la salida en la que se va a imprimir (en este caso `stderr`) y como segundo argumento el mensaje a imprimir.

También podemos imprimir información adicional usando la función `hPutStrLn` en cualquier otro lugar del código, por ejemplo para mostrar el resultado de una operación o el valor de una variable en un momento determinado.

## Profundizando en el tema

La función `hPutStrLn` acepta como primer argumento cualquier tipo de `Handle`, que representa una conexión de entrada/salida. En nuestro ejemplo, usamos `stderr`, que se refiere a la salida estándar de errores, pero también podríamos usar `stdout` para escribir a la salida estándar normal o incluso abrir un archivo y usar su `Handle` respectivo para escribir en él.

Además, existen otras funciones relacionadas como `hPutStr`, que es similar a `hPutStrLn` pero imprime sin saltar de línea, y `hPrint`, que convierte el valor en una cadena y lo imprime en la salida estándar.

## Ver también

- La documentación de la función `hPutStrLn` en el módulo `System.IO`: https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#v:hPutStrLn
- La documentación del módulo `System.IO`: https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html