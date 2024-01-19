---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Leer un archivo de texto significa tomar datos guardados en este archivo y usarlos en tu programa. Los programadores hacen esto cuando necesitan procesar información almacenada en archivos.

## Cómo hacerlo:

Vamos a leer un archivo de texto usando Gleam. Pon atención a los siguientes bloques de código:

```Gleam
import gleam/io.{File, open, read}

let file = open("archivo.txt")?
let contenido = read(file)?
```

Esto abrirá el archivo "archivo.txt", leerá su contenido y lo almacenará en la variable `contenido`.

## Inmersión profunda:

¿Por qué leemos archivos de texto? Esta práctica se remonta a los primeros días de la programación. Los archivos de texto son una forma simple y efectiva de almacenar y compartir datos. Pero no es la única opción, también existen las bases de datos y los servicios en la nube.

En Gleam, la implementación de la lectura de archivos es muy directa gracias a su naturaleza funcional. Sin embargo, este proceso puede variar dependiendo del tipo de archivo que estés leyendo y de cómo necesites procesar los datos.

## Ver también:

1. [Documentación oficial de Gleam](https://gleam.run/docs/)

2. [Gleam en GitHub](https://github.com/gleam-lang/gleam)

3. [Gleam IO (Entrada/Salida) documentación](https://hexdocs.pm/gleam_stdlib/gleam/io/index.html)