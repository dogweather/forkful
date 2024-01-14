---
title:    "Haskell: Leyendo argumentos de línea de comando"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué leer argumentos de la línea de comandos?

Hay muchas razones por las que alguien podría querer aprender a leer argumentos de la línea de comandos en Haskell. Los argumentos de la línea de comandos permiten a los usuarios proporcionar información y datos adicionales al programa, lo que lo hace más dinámico y personalizable.

## Cómo hacerlo

Para leer argumentos de la línea de comandos en Haskell, primero necesitamos importar el módulo "System.Environment". Luego, podemos utilizar la función "getArgs" para obtener una lista de todos los argumentos pasados al programa.

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn "Los argumentos que has pasado son:"
    putStrLn $ show args
```

Si ejecutamos este código con el comando `runhaskell args.hs argumento1 argumento2`, obtendremos la siguiente salida:

```
Los argumentos que has pasado son:
["argumento1","argumento2"]
```

Podemos acceder a cada argumento individualmente a través de su índice en la lista, por ejemplo, `args !! 0` para el primer argumento. También podemos utilizar patrones de asignación para asignar valores directamente a variables.

## Deep Dive

Además de la función "getArgs", también podemos utilizar la función "getProgName" para obtener el nombre del programa y la función "getEnv" para obtener variables de entorno específicas. También podemos utilizar la función "lookupEnv" para verificar la existencia de una variable de entorno antes de intentar acceder a ella.

Podemos manipular y procesar los argumentos de la línea de comandos de muchas maneras diferentes, como filtrar y transformar la lista de argumentos para obtener solo ciertos valores o utilizar patrones de coincidencia para realizar acciones específicas basadas en los argumentos proporcionados. ¡Las posibilidades son infinitas!

## Ver también

- [Documentación oficial sobre System.Environment](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Environment.html)
- [Tutorial de programación en Haskell](https://www.tutorialspoint.com/haskell/index.htm)
- [Ejemplos de programas Haskell](https://wiki.haskell.org/Example_code)