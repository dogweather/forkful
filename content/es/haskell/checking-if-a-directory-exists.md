---
title:    "Haskell: Comprobando si existe un directorio"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## ¿Por qué comprobar si un directorio existe?

Comprobar si un directorio existe es importante para garantizar que nuestras aplicaciones funcionen correctamente. Al verificar la existencia de un directorio, podemos evitar errores y evitar que nuestra aplicación falle.

## Cómo hacerlo

En Haskell, podemos utilizar la función `doesDirectoryExist` del módulo `System.Directory` para verificar si un directorio existe. Esta función devuelve un valor booleano, `True` si el directorio existe y `False` si no existe.

```
Haskell
import System.Directory (doesDirectoryExist)
main = do
    dirExists <- doesDirectoryExist "/ruta/al/directorio"
    if dirExists
        then putStrLn "El directorio existe."
        else putStrLn "El directorio no existe."
```

Si ejecutamos este código, obtendremos la siguiente salida:

```
El directorio existe.
```

También podemos utilizar la función `doesDirectoryExist` con una variable que contenga la ruta del directorio:

```
Haskell
import System.Directory (doesDirectoryExist)
main = do
    let dir = "/ruta/al/directorio"
    dirExists <- doesDirectoryExist dir
    if dirExists
        then putStrLn ("El directorio " ++ dir ++ " existe.")
        else putStrLn ("El directorio " ++ dir ++ " no existe.")
```

La salida será similar a la anterior:

```
El directorio /ruta/al/directorio existe.
```

## Profundizando

Además de la función `doesDirectoryExist`, el módulo `System.Directory` también contiene otras funciones útiles para trabajar con directorios, como `createDirectory`, que crea un nuevo directorio, y `getDirectoryContents`, que devuelve una lista de nombres de archivos y directorios en una ruta determinada.

Es importante tener en cuenta que la función `doesDirectoryExist` solo verifica si un directorio existe, no realiza ninguna acción de creación de directorios si el directorio no existe. Por lo tanto, es importante manejar esto adecuadamente en nuestro código para garantizar que los directorios que necesitamos estén creados antes de acceder a ellos.

## Ver también

- [Documentación oficial de Haskell sobre el módulo System.Directory](https://hackage.haskell.org/package/directory-1.3.5.0/docs/System-Directory.html)
- [Tutorial de Learn You a Haskell sobre trabajar con archivos y directorios](http://learnyouahaskell.com/input-and-output#files-and-streams)