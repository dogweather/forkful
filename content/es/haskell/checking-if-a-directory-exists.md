---
title:                "Comprobando si existe un directorio"
date:                  2024-01-20T14:56:48.891592-07:00
html_title:           "Gleam: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Revisar si un directorio existe te permite confirmar su presencia antes de interactuar con él, como leer o escribir archivos. Los programadores lo hacen para evitar errores como 'directorio no encontrado' y manejar estos casos graciosamente.

## Cómo hacerlo:

```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dir = "path/to/your/directory"
  exists <- doesDirectoryExist dir
  putStrLn $ "El directorio " ++ (if exists then "existe." else "no existe.")
```

Ejecutar este programa te dará una línea de salida que dice `El directorio existe.` o `El directorio no existe.` dependiendo de la realidad del directorio que estás revisando.

## Profundización

Historicamente, revisar si un directorio existe era más complicado y osado de errores. La función `doesDirectoryExist` de la librería `System.Directory` simplifica esta tarea, formando parte de las prácticas seguras y modernas en Haskell.

Alternativamente, podrías usar la función `getDirectoryContents` para enumerar archivos y directorios y luego verificar si tu directorio está en la lista; sin embargo, `doesDirectoryExist` es más directo y eficiente.

Internamente, `doesDirectoryExist` hace una syscall para obtener la información del sistema de archivos, manejando adecuadamente los permisos y errores posibles, lo que hace que sea confiable y recomendable para este propósito.

## Ver También

- La documentación de Haskell para el módulo `System.Directory`: 

  [System.Directory en Hackage](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)

- Artículo sobre el manejo de archivos y directorios en Haskell:

  [Learn You a Haskell for Great Good! - Input and Output](http://learnyouahaskell.com/input-and-output#files-and-streams) 

- Preguntas frecuentes sobre el manejo de directorios en StackOverflow:

  [Haskell on StackOverflow](https://stackoverflow.com/questions/tagged/haskell+directory)