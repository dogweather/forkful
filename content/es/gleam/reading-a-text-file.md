---
title:    "Gleam: Leyendo un archivo de texto."
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué leer un archivo de texto en Gleam

Leer un archivo de texto es una tarea común en la programación. Puede ser útil para leer datos de entrada, procesar archivos de registro o simplemente para obtener información almacenada en un archivo. En Gleam, esto se puede hacer de manera sencilla y eficiente.

## Cómo hacerlo

Para leer un archivo de texto en Gleam, primero debemos importar el módulo `gleam/io`.

```Gleam
import gleam/io
```

A continuación, podemos usar la función `read_file` para leer el archivo de texto. Esta función acepta como argumento una ruta de archivo y devuelve una tupla con un resultado exitoso o un error.

```Gleam
result = io.read_file("my_file.txt")
```

Si el archivo se lee correctamente, podemos acceder al contenido del archivo a través del resultado exitoso.

```Gleam
case result {
  Ok(contents) -> {
    // hacer algo con el contenido del archivo
  }
  Err(err) -> {
    // manejar el error
  }
}
```

Si el archivo no se puede leer, el patrón `Err` se obtendrá junto con el mensaje de error correspondiente.

## Profundizando

La función `read_file` utiliza una función de bajo nivel llamada `read` que acepta un descriptor de archivo y devuelve una cadena de bytes. Esto hace que sea posible leer archivos binarios además de archivos de texto.

También es posible especificar opciones adicionales al leer el archivo, como el número máximo de bytes a leer o el desplazamiento desde donde empezar a leer.

En resumen, leer un archivo de texto en Gleam es sencillo y ofrece opciones adicionales para adaptarse a diferentes necesidades.

## Ver también
- [Documentación del módulo `gleam/io`](https://gleam.run/modules/gleam_io.html)
- [Tutorial de Gleam](https://gleam.run/book/tour.html)
- [Ejemplos de código de Gleam](https://github.com/gleam-lang/examples)