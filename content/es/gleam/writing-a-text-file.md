---
title:                "Gleam: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

# ¿Por qué escribir un archivo de texto en Gleam?

Escribir un archivo de texto en Gleam puede ser una tarea útil y necesaria al trabajar en proyectos de programación. Al guardar información en un archivo de texto, podemos acceder a ella fácilmente en el futuro y compartirla con otros desarrolladores.

## Cómo hacerlo

Primero, debemos importar el módulo `gleam/fs` para tener acceso a las funciones de manejo de archivos. Luego, podemos utilizar la función `fs.write_file` para escribir un archivo de texto. Esta función toma dos argumentos: la ruta del archivo y el contenido que queremos escribir. Por ejemplo:

```Gleam
import gleam/fs

fs.write_file("ejemplo.txt", "Este es un archivo de ejemplo")
```

Este código creará un archivo llamado "ejemplo.txt" en el mismo directorio donde se encuentra el archivo de código fuente. El contenido del archivo será "Este es un archivo de ejemplo".

También podemos utilizar la función `fs.append_file` para agregar contenido a un archivo existente. Esta función toma los mismos argumentos que `fs.write_file`.

## Inmersión profunda

Al escribir un archivo de texto en Gleam, es importante tener en cuenta la codificación del archivo. Por defecto, los archivos se guardan en UTF-8, pero si necesitamos utilizar una codificación diferente, podemos usar la función `fs.write_file_with_encoding`. Esta función toma un tercer argumento que especifica la codificación que queremos utilizar.

Además, es importante cerrar el archivo después de escribir en él utilizando la función `fs.close_file`. De esta manera, nos aseguramos de que el archivo sea guardado correctamente y que no haya problemas de permisos al intentar acceder nuevamente al archivo.

# Ver también

- Documentación oficial de Gleam sobre el módulo `gleam/fs`: [https://gleam.run/documentation/stdlib/fs/](https://gleam.run/documentation/stdlib/fs/)
- Tutorial de Gleam en español: [https://gleam.run/es/tutorials/](https://gleam.run/es/tutorials/)
- Repositorio de código fuente de Gleam: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)