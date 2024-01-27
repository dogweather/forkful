---
title:                "Escritura de un archivo de texto"
date:                  2024-01-19
html_title:           "Bash: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir un archivo de texto consiste en guardar texto en un archivo en tu sistema de archivos. Los programadores hacen esto para persistir datos, configuraciones, o para exportar información que luego otras aplicaciones o usuarios pueden leer.

## Cómo hacerlo:
En Gleam, escribir un archivo de texto es sencillo. Aquí tienes un ejemplo básico:

```gleam
import gleam/io
import gleam/erlang.{File}

fn escribe_archivo() {
  let resultado = File.open("salida.txt", [File.write]) // Abre el archivo en modo escritura
  case resultado {
    Ok(file) -> 
      io.write(file, "¡Hola, Gleam!") // Escribe en el archivo
      File.close(file)                // Cierra el archivo
    Error(error) ->
      io.println("Error al abrir archivo: " ++ error)
  }
}
```

Si corres este código, creará un archivo llamado `salida.txt` con el contenido "¡Hola, Gleam!".

## Profundizando
Históricamente, la escritura de archivos viene desde los primeros días de la computación, inicialmente en tarjetas perforadas. Hoy, aunque hay muchas maneras de persistir datos, como bases de datos o almacenamiento en la nube, la escritura de archivos de texto sigue siendo una forma simple y directa de almacenar información.

Alternativas a `gleam/erlang.File` incluyen trabajar con bibliotecas específicas que facilitan la serialización de datos como JSON o XML para estructuras más complejas. También, dependiendo del sistema operativo y del entorno, podrías querer utilizar APIs nativas para funcionalidades más avanzadas. 

En Gleam, el manejo de archivos se basa en las operaciones de entrada/salida de Erlang, que está diseñado para trabajar eficientemente con su modelo concurrente y distribuido. Es importante cerrar los archivos para no desperdiciar recursos del sistema.
