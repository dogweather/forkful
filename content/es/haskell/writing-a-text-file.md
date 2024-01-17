---
title:                "Escribiendo un archivo de texto"
html_title:           "Haskell: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir un archivo de texto simplemente significa guardar información en un formato que pueda ser leído por una computadora. Los programadores a menudo lo hacen para almacenar datos o configuraciones de sus programas de manera conveniente.

## Cómo:

### Ejemplo 1: Escribiendo un archivo de texto

```Haskell
import System.IO

main = do
    let fileName = "mi_archivo.txt"
    let fileContent = "Hola a todos!"
    writeFile fileName fileContent
```

### Ejemplo 2: Añadiendo texto a un archivo existente

```Haskell
import System.IO

main = do
    let fileName = "mi_archivo.txt"
    let additionalContent = "\n¡Hola de nuevo!"
    appendFile fileName additionalContent
```

## Profundizando:

Escribir archivos de texto es una tarea común en la programación, ya que proporciona una forma sencilla de almacenar información de manera persistente. También es posible leer datos de archivos de texto utilizando funciones como `readFile` o `openFile`.

Otra alternativa para almacenar información es utilizar una base de datos, pero a veces un simple archivo de texto puede ser suficiente para nuestras necesidades.

Además de los ejemplos mostrados anteriormente, también es importante tener en cuenta que es necesario asegurar que el archivo se cierre correctamente después de usarlo.

## Ver también:

- Documentación de System.IO: https://hackage.haskell.org/package/base/docs/System-IO.html
- Tutorial básico de Haskell: https://www.haskell.org/tutorial/