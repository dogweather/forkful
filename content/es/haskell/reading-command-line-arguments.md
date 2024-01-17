---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "Haskell: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué lo hacemos?

Leer argumentos de línea de comando es una técnica utilizada por los programadores para permitir que sus programas acepten información directamente desde la consola de comandos. Esto es útil para personalizar la ejecución de un programa y para proporcionar una entrada rápida de datos. 

## Cómo hacerlo:

Para leer argumentos de línea de comando en Haskell, se puede utilizar la función `getArgs` del módulo System.Environment. Esta función devuelve una lista de cadenas (strings) con cada uno de los argumentos pasados al programa. Por ejemplo:

```Haskell
import System.Environment (getArgs)

main = do
    args <- getArgs
    putStrLn ("Hola " ++ args !! 0 ++ "!")
```

Si ejecutamos este programa con `runhaskell Saludar.hs María`, obtendríamos como salida `Hola María!`, ya que `"María"` ha sido pasada como argumento al programa. 

## Profundizando:

Leer argumentos de línea de comando es una práctica común en la programación, especialmente en aplicaciones de línea de comandos o en programas que deben aceptar diferentes opciones de configuración. Usualmente, los argumentos se pueden pasar en cualquier orden y también pueden incluir banderas (flags) para especificar ciertas opciones. 

Otra forma de obtener argumentos de línea de comando en Haskell es utilizando la función `getProgName` del mismo módulo. Esta función devuelve una cadena con el nombre del programa en ejecución. 

## Enlaces relacionados:

- Documentación oficial de la función `getArgs`: https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html#v:getArgs
- Ejemplos de programas en Haskell que utilizan argumentos de línea de comando: https://wiki.haskell.org/How_to_write_a_Haskell_program
- Opciones de compilación en Haskell para trabajar con argumentos de línea de comando: https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html#v:getArgs