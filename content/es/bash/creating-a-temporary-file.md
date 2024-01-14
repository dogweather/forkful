---
title:                "Bash: Title: Creando un archivo temporal"
simple_title:         "Title: Creando un archivo temporal"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Por qué crear un archivo temporal en Bash?

Crear un archivo temporal en Bash es una práctica común entre los programadores para realizar ciertas tareas en un entorno seguro y controlado. Al crear un archivo temporal, se asegura que no habrá conflictos con otros archivos o procesos y se pueden realizar cambios sin afectar directamente los archivos existentes en el sistema.

## Cómo crear un archivo temporal en Bash

Existen varias formas de crear un archivo temporal en Bash, pero en este blog post te mostraré dos de las más comunes.

Primero, puedes utilizar el comando `mktemp` seguido de la ruta y el nombre del archivo temporal que deseas crear. Por ejemplo:

```Bash
temp_file=$(mktemp /tmp/miarchivo.XXXX)
```

Esto creará un archivo temporal llamado "miarchivo" en la carpeta "/tmp" con una extensión aleatoria de 4 caracteres. Puedes utilizar el archivo `temp_file` para realizar cualquier operación que necesites dentro de tu script de Bash.

Otra opción es utilizar el comando `touch` y redirigir su salida hacia un archivo temporal utilizando el operador ">" en lugar de un nombre de archivo. Por ejemplo:

```Bash
touch > /tmp/miarchivo
```

Esto creará un archivo temporal vacío en la carpeta "/tmp". Puedes utilizar este archivo para escribir o copiar datos antes de eliminarlo al final del script.

## Profundizando en la creación de archivos temporales en Bash

Crear un archivo temporal en Bash puede ser útil en muchas situaciones, como por ejemplo cuando necesitas almacenar datos temporales mientras realizas operaciones o cuando trabajas con archivos grandes y necesitas fragmentarlos en archivos más pequeños para su procesamiento.

Además, puedes especificar el nombre, la ruta y la extensión del archivo temporal utilizando varias opciones y modificadores con los comandos `mktemp` y `touch`, lo que te permite personalizar el archivo según tus necesidades.

Un punto importante a tener en cuenta es que al finalizar tu script o proceso, debes eliminar los archivos temporales creados para evitar una acumulación innecesaria de archivos en el sistema.

## Ver también

- [Tutorial de Bash (en español)](https://www.hostinger.es/tutoriales/comandos-de-linux/)
- [Documentación de mktemp](https://man7.org/linux/man-pages/man1/mktemp.1.html)
- [Documentación de touch](https://man7.org/linux/man-pages/man1/touch.1.html)