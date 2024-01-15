---
title:                "Escribir un archivo de texto"
html_title:           "Haskell: Escribir un archivo de texto"
simple_title:         "Escribir un archivo de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir un archivo de texto?

Escribir un archivo de texto en Haskell puede ser útil para almacenar y manipular datos de manera eficiente. Además, también puede ser utilizado para crear archivos de configuración o para guardar resultados de procesamiento de datos.

## Cómo hacerlo

Para escribir un archivo de texto en Haskell, primero debemos importar el módulo `System.IO` que nos permite interactuar con el sistema de archivos. Luego, utilizaremos la función `writeFile` para escribir en el archivo. Veamos un ejemplo:

```Haskell
import System.IO

main = do
    let texto = "¡Hola mundo!"
    writeFile "archivo.txt" texto
```

En este ejemplo, hemos creado una variable `texto` con el texto que queremos escribir en el archivo y luego utilizamos la función `writeFile` para escribirlo en un archivo llamado "archivo.txt". Si ejecutamos este código, se creará un archivo de texto con el contenido "¡Hola mundo!"

## Profundizando

En el ejemplo anterior, utilizamos la función `writeFile` para escribir en un archivo, pero también existen otras funciones que nos permiten interactuar con archivos de texto en Haskell.

Por ejemplo, la función `appendFile` nos permite agregar contenido a un archivo existente en lugar de sobrescribirlo. También podemos utilizar la función `hPutStrLn` para escribir en un archivo a través de un handle, que es una referencia a un archivo abierto.

Es importante tener en cuenta que cuando trabajamos con archivos en Haskell, debemos utilizar la función `openFile` para abrirlos y luego cerrarlos utilizando la función `hClose` una vez que hayamos terminado de trabajar con ellos.

## Ver también

- [Documentación de System.IO](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Tutorial de Haskell en Español](https://wiki.haskell.org/Tutorial_in_Spanish)