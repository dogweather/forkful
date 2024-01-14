---
title:                "Haskell: Comprobando si existe un directorio"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador de Haskell, seguramente te has enfrentado a la tarea de verificar si un directorio existe antes de realizar alguna operación en él. Aunque puede parecer un paso innecesario, es importante asegurarse de que el directorio en cuestión realmente existe antes de intentar acceder o manipular sus contenidos.

## Cómo hacerlo

Para verificar si un directorio existe en Haskell, podemos utilizar la función `doesDirectoryExist` del módulo `System.Directory`. Esta función acepta una ruta de directorio como argumento y devuelve un valor booleano que indica si el directorio existe o no.

```Haskell
import System.Directory

ruta <- getLine
existe <- doesDirectoryExist ruta
print existe
```

Si ejecutamos este código y le proporcionamos una ruta de directorio existente, veremos que la salida será `True`. En caso contrario, la salida será `False`.

## Profundizando

Ahora que sabemos cómo verificar si un directorio existe, podemos profundizar un poco más en el tema. ¿Qué sucede si la ruta especificada no es un directorio, sino un archivo? En este caso, la función `doesDirectoryExist` también devolverá `True`. Esto se debe a que, según la documentación del módulo `System.Directory`, la función simplemente comprueba si el componente final de la ruta es un directorio o no.

Sin embargo, si realmente queremos verificar si la ruta específicada corresponde a un directorio y no a un archivo, podemos utilizar la función `doesFileExist` del mismo módulo. Esta función también acepta una ruta como argumento y devuelve un valor booleano, pero esta vez indicando si el componente final de la ruta es un archivo o no.

```Haskell
import System.Directory

ruta <- getLine
existe <- doesFileExist ruta
print existe
```

De manera similar a la función anterior, la salida será `True` si la ruta especificada corresponde a un archivo y `False` en caso contrario.

## Ver también

- Documentación oficial de `System.Directory`: https://hackage.haskell.org/package/directory/docs/System-Directory.html
- Tutorial de Haskell: https://learnxinyminutes.com/docs/es-es/haskell-es/
- Repositorio de ejemplos de Haskell: https://github.com/bitemyapp/learnhaskell