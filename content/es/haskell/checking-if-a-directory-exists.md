---
title:                "Comprobando si existe un directorio"
html_title:           "Haskell: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué
Comprobar si un directorio existe es una tarea común en la programación, ya sea para validar rutas de archivos o para realizar operaciones en diferentes ubicaciones. En este artículo aprenderás cómo hacerlo en Haskell.

## Cómo hacerlo
En Haskell, para comprobar si un directorio existe, utilizamos la función `doesDirectoryExist` del módulo System.Directory. Esta función toma como parámetro una ruta de directorio y devuelve un valor booleano indicando si el directorio existe o no.

```Haskell
import System.Directory (doesDirectoryExist)

main = do
  let directorio1 = "/Users/Usuario/Escritorio"
      directorio2 = "/Users/Usuario/Música"
  existeDir1 <- doesDirectoryExist directorio1
  existeDir2 <- doesDirectoryExist directorio2
  putStrLn $ "¿El directorio " ++ directorio1 ++ " existe? " ++ show existeDir1
  putStrLn $ "¿El directorio " ++ directorio2 ++ " existe? " ++ show existeDir2
```

El código anterior comprobará si los directorios "Escritorio" y "Música" existen en la ruta del usuario y mostrará un mensaje de texto indicando si existen o no. Podemos ver que utilizamos la sintaxis `do` para realizar acciones en el orden que las escribimos y `show` para convertir el valor booleano en un String legible.

## Deep Dive
En caso de que necesitemos realizar otras operaciones sobre el directorio existente, como por ejemplo, listar sus archivos o comprobar sus permisos, podemos utilizar la función `getDirectoryContents` del mismo módulo. Esta función devuelve una lista de Strings con los nombres de todos los archivos y subdirectorios contenidos en el directorio especificado.

```Haskell
import System.Directory (getDirectoryContents)

main = do
  let directorio = "/Users/Usuario/Descargas"
  contenido <- getDirectoryContents directorio
  putStrLn $ "El directorio " ++ directorio ++ " tiene los siguientes contenidos:"
  mapM_ putStrLn contenido
```

En este ejemplo, usamos la función `mapM_` para imprimir cada elemento de la lista en una nueva línea. También podemos utilizar la función `getPermissions` para obtener los permisos del directorio y realizar comprobaciones adicionales.

## Ver también
- [Documentación de System.Directory en Hackage](https://hackage.haskell.org/package/directory)
- [Tutorial de Haskell en español](https://www.haskell.org/tutorials/tutorialsp.html)