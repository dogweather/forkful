---
title:                "Gleam: Leyendo un archivo de texto"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces, como programadores, nos encontramos con la necesidad de leer archivos de texto para realizar diferentes tareas. Esto puede ser desde leer un archivo de configuración hasta procesar una gran cantidad de datos. En este post, te explicaremos por qué es importante aprender a leer archivos de texto en Gleam y cómo puedes hacerlo de manera eficiente.

## Cómo leer un archivo de texto en Gleam

Para leer un archivo de texto en Gleam, utilizamos la función `File.read` pasando como argumento la ruta del archivo que queremos leer. A continuación, podemos utilizar la función `String.split` para dividir el contenido del archivo en líneas y así poder acceder a cada una de ellas. Veamos un ejemplo:

```Gleam
let file_content = File.read("ejemplo.txt") //Lee el archivo "ejemplo.txt"
let lines = String.split(file_content, "\n") //Divide el contenido del archivo en líneas
```

Ahora, podemos imprimir cada línea utilizando un ciclo `for` y la función `IO.puts`:

```Gleam
for line in lines {
    IO.puts(line) //Imprime cada línea en la consola
}
```

Si ejecutas este código, podrás ver en la consola el contenido del archivo de texto. ¡Genial!

## Un vistazo más profundo

Además de leer y procesar archivos de texto, Gleam también ofrece diferentes funciones para trabajar con el contenido de estos archivos. Por ejemplo, podemos utilizar la función `String.substr` para obtener un fragmento específico del texto, o la función `String.trim` para eliminar espacios en blanco al principio y al final de una línea.

También es importante mencionar que podemos especificar un modo de lectura al abrir el archivo utilizando la función `File.open`. Esto nos permite tener más control sobre cómo se lee el archivo, como por ejemplo, leer solo una cierta cantidad de bytes o saltar ciertas líneas.

## Ver también

- [Documentación oficial de Gleam sobre lectura de archivos](https://gleam.run/documentation/libraries/files/#Reading-files)
- [Artículo sobre manipulación de archivos en Gleam](https://medium.com/@erikschierboom/handling-files-in-gleam-9416fd5f0d8)
- [Ejemplo de lectura y escritura de archivos de texto en Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/files/read_and_write_text_file.gleam)