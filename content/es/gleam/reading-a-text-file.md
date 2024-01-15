---
title:                "Leer un archivo de texto"
html_title:           "Gleam: Leer un archivo de texto"
simple_title:         "Leer un archivo de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto?

Leer un archivo de texto es una tarea común en la programación, ya sea para obtener datos o para manipular la información contenida en él. Aprender cómo hacerlo en Gleam puede mejorar tus habilidades de programación y ampliar tus posibilidades al trabajar con archivos.

## Cómo hacerlo

Para leer un archivo de texto en Gleam, primero debes importar el módulo `io` y usar la función `read_file` para obtener el contenido del archivo. Por ejemplo:

```Gleam
import io

let archivo = io.read_file("datos.txt")

io.println(archivo) // Imprimirá el contenido del archivo
```

En el código anterior, utilizamos la función `read_file` para leer el contenido del archivo "datos.txt" y lo asignamos a una variable llamada "archivo". Luego, utilizamos la función `println` del módulo `io` para imprimir el contenido del archivo en la consola.

Además de leer un archivo completo, también puedes leer líneas específicas utilizando la función `read_line` y especificando el número de línea que deseas leer. Por ejemplo:

```Gleam
let linea3 = io.read_line("datos.txt", 3) // Leerá la tercera línea del archivo
```

También puedes utilizar el operador `|>` para encadenar varias funciones y operar en el contenido del archivo de texto de manera más flexible. Por ejemplo:

```Gleam
let contenido = io.read_file("datos.txt")
    |> String.trim // Eliminará los espacios en blanco al principio y al final del contenido
    |> String.split("\n") // Dividirá el contenido en una lista por cada salto de línea
    |> List.to_map // Convertirá la lista en un mapa con cada línea como clave y el contenido como valor
```

## Profundizando

Aunque leer un archivo de texto en Gleam es una tarea sencilla, es importante tener en cuenta algunos detalles:

- La función `read_file` puede recibir un segundo argumento opcional que especifica la codificación del archivo, si es diferente a UTF-8.
- Si el archivo no existe o no tiene permisos de lectura, se lanzará una excepción que deberás manejar adecuadamente.
- Puedes utilizar la función `read_file_or_none` para leer un archivo sin lanzar una excepción si no existe, devolviendo `None` en su lugar.

## Ver también

- [Documentación de io en el sitio web de Gleam](https://gleam.run/modules/io.html)
- [Ejemplos de lectura de archivos de texto en Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/io/text_file_reader.gleam)