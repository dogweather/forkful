---
title:    "Haskell: Comprobando si existe un directorio"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez has necesitado asegurarte de que un directorio existe antes de realizar una operación en él? Ya sea creando un archivo, eliminando un directorio o simplemente consultando su contenido, es importante comprobar si un directorio existe antes de continuar con tu programa. Afortunadamente, Haskell tiene una manera fácil de hacerlo. ¡Sigue leyendo para aprender cómo!

## Cómo

En Haskell, podemos usar la función `doesDirectoryExist` del módulo `System.Directory` para verificar si un directorio existe en una ruta específica. Esta función toma un `FilePath`, que es simplemente una cadena que representa la ruta al directorio que queremos verificar. Aquí hay un ejemplo de cómo usarlo:

```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let path = "/home/usuario/documentos/"
  exists <- doesDirectoryExist path
  if exists
    then putStrLn "El directorio existe!"
    else putStrLn "El directorio no existe :("
```

En este ejemplo, primero importamos el módulo `System.Directory` y luego definimos una ruta al directorio que queremos verificar. Luego, utilizamos la función `doesDirectoryExist` para comprobar si ese directorio existe. La función devuelve un valor booleano, por lo que podemos usarlo en una expresión condicional para imprimir un mensaje. Si el directorio existe, imprimiremos "El directorio existe!" de lo contrario, imprimirá "El directorio no existe :(". 

## Deep Dive

Puedes estar preguntándote cómo funciona exactamente la función `doesDirectoryExist` y cómo puede saber si un directorio existe o no. La respuesta está en la implementación de Haskell de `FilePath`, que es una simple cadena de caracteres, y cómo se manejan las rutas de los archivos en diferentes sistemas operativos.

Haskell utiliza la función `getPermissions` para determinar si la ruta pasada como argumento existe o no. Esta función devuelve un `Permissions` que representa los permisos del archivo o directorio en cuestión. Si la ruta existe, `getPermissions` devolverá un `Permissions` que indica que el archivo o directorio tiene permisos de lectura, escritura y ejecución. Por otro lado, si la ruta no existe, la función devolverá un error.

`doesDirectoryExist` simplemente utiliza la función `getPermissions` y verifica si hubo un error o no. Si no hay errores, entonces sabemos que el directorio existe.

## Ver también
- [Documentación oficial de `System.Directory`](https://www.haskell.org/cabal/users-guide/developing-packages.html)
- [Otros módulos de `System.Directory`](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)
- [Más información sobre `FilePath`](https://www.haskell.org/onlinereport/haskell2010/haskellch9.html)