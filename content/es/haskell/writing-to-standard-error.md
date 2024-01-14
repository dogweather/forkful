---
title:    "Haskell: Escribiendo en la salida de error estándar"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por Qué

Escribir a la salida de error estándar puede ser una herramienta útil para depurar y manejar errores en tus programas de Haskell. Al escribir a esta salida, puedes ver información detallada sobre lo que está sucediendo en tu código y cómo manejar posibles errores.

## Cómo

Para escribir a la salida de error estándar en Haskell, puedes utilizar la función `hPutStrLn` del módulo `System.IO`. Esta función toma dos parámetros: un identificador de archivo y una cadena de texto. Aquí hay un ejemplo de cómo usarla:

```Haskell
import System.IO

main = do
  hPutStrLn stderr "Este es un mensaje de error"
```

Este código escribirá "Este es un mensaje de error" en la salida de error estándar. Puedes cambiar la cadena de texto por cualquier mensaje que quieras mostrar.

Aquí hay otro ejemplo más complejo que ilustra cómo puedes manejar errores mientras escribes a la salida de error estándar:

```Haskell
import System.IO
import Control.Exception

main = do
  let fileName = "mi_archivo.txt"
  contents <- readFile fileName
  putStr contents
  `catch` \e -> do
    let err = show (e :: IOException)
    hPutStrLn stderr ("Error al abrir el archivo: " ++ fileName ++ "\n" ++ err)
```

En este ejemplo, tratamos de leer un archivo llamado "mi_archivo.txt" y mostrar su contenido por la salida estándar. Sin embargo, si el archivo no existe, se lanzará una excepción y nuestro código capturará ese error y lo imprimirá en la salida de error estándar. Esto puede ser útil para manejar errores inesperados en tus programas y mantener un código más robusto.

## Profundizando

Para una explicación más detallada sobre cómo escribir a la salida de error estándar en Haskell, puedes consultar la documentación oficial del módulo `System.IO` y la función `hPutStrLn`. También puedes investigar sobre el manejo de errores en Haskell y cómo puedes utilizarlo en tus programas.

## Ver también

- Documentación del módulo `System.IO`: [https://hackage.haskell.org/package/base/docs/System-IO.html](https://hackage.haskell.org/package/base/docs/System-IO.html)
- Documentación de la función `hPutStrLn`: [https://hackage.haskell.org/package/base/docs/System-IO.html#v:hPutStrLn](https://hackage.haskell.org/package/base/docs/System-IO.html#v:hPutStrLn)
- Tutoriales sobre manejo de errores en Haskell: [https://www.tutorialspoint.com/haskell/haskell_error_handling.htm](https://www.tutorialspoint.com/haskell/haskell_error_handling.htm)