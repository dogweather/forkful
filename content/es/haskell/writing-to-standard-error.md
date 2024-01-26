---
title:                "Escribiendo en el error estándar"
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qué es y por qué?
Escribir en el error estándar (stderr) es dirigir tus mensajes de error a un stream específico, diferente del output normal (stdout). Los programadores hacen esto para separar los logs normales de los errores, facilitando la depuración y el manejo de errores.

## Cómo hacerlo:
Haskell maneja el output estándar y el error estándar a través de módulos de la librería `System.IO`. La función `hPutStrLn` permite escribir en stderr:

```Haskell
import System.IO (stderr, hPutStrLn)

main :: IO ()
main = do
   hPutStrLn stderr "Este es un mensaje de error"
```
Salida en terminal cuando hay un error:
```
Este es un mensaje de error
```

## Profundización:
Históricamente, la separación de stdout y stderr proviene de Unix: stdout para datos resultantes y stderr para mensajes de error. En Haskell, `System.IO` permite trabajar con estos streams; puedes usar `hPrint` para datos más complejos. `stderr` es parte de un conjunto más grande de manejadores de archivo predefinidos que incluye también `stdin` y `stdout`.

## Ver También:
- [Haskell System.IO library](https://hackage.haskell.org/package/base-4.16.1.0/docs/System-IO.html)
- [A detailed guide to streams in Unix](https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr))
- [Haskell Wiki entry on IO](https://wiki.haskell.org/IO_inside)
