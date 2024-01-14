---
title:    "Haskell: Leyendo un archivo de texto."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto en Haskell?

Leer un archivo de texto puede ser una tarea muy útil en programación, especialmente en el lenguaje Haskell. Ya sea para obtener datos de entrada, procesar información o guardar resultados, la lectura de archivos de texto es una herramienta esencial en el desarrollo de aplicaciones. En este artículo, aprenderemos cómo leer un archivo de texto en Haskell y profundizaremos en cómo funciona esta función.

## Cómo leer un archivo de texto en Haskell

Para leer un archivo de texto en Haskell, utilizaremos la función `readFile` de la biblioteca estándar `System.IO`. Esta función toma como argumento la ruta del archivo a leer y devuelve una cadena con el contenido del archivo. Veámoslo en acción mediante un ejemplo de código:

```Haskell
import System.IO

main = do
    contenido <- readFile "archivo.txt"
    putStrLn contenido
```

En este ejemplo, importamos el módulo `System.IO` y utilizamos la función `readFile` en la línea `contenido <- readFile "archivo.txt"`. La expresión `contenido <-` indica que asignamos el resultado de `readFile` a la variable `contenido`. Luego, utilizamos la función `putStrLn` para imprimir el contenido del archivo en la consola.

## Profundizando en la lectura de archivos de texto

Al leer un archivo de texto en Haskell, es importante tener en cuenta cómo se manejan los errores. La función `readFile` puede lanzar una excepción si la ruta del archivo no existe o si no tenemos permiso para leer el archivo. Para manejar estas situaciones, podemos utilizar la función `catch` del módulo `Control.Exception`. Por ejemplo:

```Haskell
import System.IO
import Control.Exception

main = do
    contenido <- catch (readFile "archivo.txt") (\e -> return $ show (e :: IOException))
    putStrLn contenido
```

En este código, utilizamos la función `catch` para capturar cualquier excepción que pueda lanzar `readFile`. Si se produce una excepción, la función `catch` ejecuta la expresión después de la flecha, en este caso `return $ show (e :: IOException)`, que devuelve una cadena con información sobre el error. De esta manera, nuestro programa maneja adecuadamente cualquier excepción y puede imprimir un mensaje de error en lugar de fallar.

## Véase también

- [Documentación de la función `readFile` en Haskell](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html#v:readFile)
- [Documentación de la función `catch` en Haskell](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#v:catch)