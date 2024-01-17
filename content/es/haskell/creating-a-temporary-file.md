---
title:                "Creando un archivo temporal"
html_title:           "Haskell: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Crear un archivo temporal se refiere a crear un archivo que solo existirá temporalmente durante la ejecución del programa. Los programadores suelen hacer esto para almacenar datos temporales o para realizar pruebas sin afectar a archivos existentes.

## ¿Cómo hacerlo?
En Haskell, podemos crear un archivo temporal utilizando la función `withSystemTempFile` del módulo `System.IO.Temp`. Esta función toma dos parámetros: una cadena de texto con el nombre base del archivo y una función que utilizará el archivo temporal. La función devolverá el resultado de la función utilizada en el archivo temporal. Por ejemplo:

```Haskell 
import System.IO.Temp (withSystemTempFile)
main = withSystemTempFile "tempFile" $ \filePath handle -> do
  putStrLn $ "Archivo temporal creado en: " ++ filePath
  hPutStrLn handle "Este es un texto temporal"
```

La salida de este código sería:
```
Archivo temporal creado en: /tmp/tempFile1810772932
```
Y el contenido del archivo temporal sería el texto "Este es un texto temporal".

## Deep Dive
Crear archivos temporales es una práctica común en programación, ya que nos permite manipular datos temporales sin afectar a archivos importantes. Además, también puede ser utilizado para realizar pruebas sin correr el riesgo de alterar archivos ya existentes.

Otra forma de crear archivos temporales en Haskell es utilizando la función `openTempFile`. A diferencia de `withSystemTempFile`, esta función requiere que cierres el archivo manualmente utilizando la función `hClose handle`.

Además, también podemos especificar una ruta para el archivo temporal utilizando `openTempFile`. Por defecto, `withSystemTempFile` creará el archivo en la carpeta temporal del sistema.

## Ver también
- Documentación oficial de `System.IO.Temp` en Haskell: https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO-Temp.html
- Cómo crear y manipular archivos en Haskell: https://www.tutorialspoint.com/haskell/haskell_files_io.htm