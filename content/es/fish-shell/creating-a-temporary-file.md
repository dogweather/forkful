---
title:                "Fish Shell: Creación de un archivo temporal"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Por qué crear un archivo temporal en Fish Shell?

La creación de archivos temporales es una práctica común en la programación, especialmente en lenguajes de script como Fish Shell. Los archivos temporales sirven como "espacios de trabajo" para almacenar datos temporales o para realizar operaciones intermedias en un programa. En este artículo, aprenderemos cómo crear un archivo temporal en Fish Shell y cómo aprovecharlo en tus proyectos.

## Cómo hacerlo

Para crear un archivo temporal en Fish Shell, podemos utilizar el comando `mktemp` seguido de la opción `-t` y un nombre de prefijo para el archivo temporal. Por ejemplo, si queremos crear un archivo temporal llamado "ejemplo_temporal", podemos escribir el siguiente comando en nuestra terminal:

```
mktemp -t ejemplo_temporal
```

Esto creará automáticamente un archivo con el nombre "ejemplo_temporal" seguido de un código único generado por `mktemp`. Una vez que hayamos terminado de usar el archivo temporal, podemos eliminarlo con el comando `rm`.

Podemos utilizar este archivo temporal para almacenar datos temporales o para realizar operaciones intermedias en nuestro programa. Por ejemplo, podemos utilizarlo como un archivo de registro para guardar la salida de un comando antes de procesarla más adelante.

```
mktemp -t log > salida.tmp
# Procesar salida.tmp de alguna forma ...
# Eliminar el archivo temporal al final:
rm salida.tmp
```

## Profundizando

Aunque `mktemp` es la forma más sencilla de crear un archivo temporal en Fish Shell, también podemos utilizar el comando `touch` para crear un archivo vacío y luego renombrarlo a un nombre de archivo temporal generado por `mktemp`.

Además, podemos utilizar la opción `-p` junto con `mktemp` para especificar una ubicación para el archivo temporal. Esto puede ser útil si queremos almacenar el archivo temporal en una carpeta específica. Por ejemplo:

```
mktemp -p ruta/directorio -t archivo_temporal
```

También es importante tener en cuenta que los archivos temporales no se eliminan automáticamente después de su uso. Es nuestra responsabilidad como programadores asegurarnos de eliminarlos correctamente después de usarlos para evitar problemas de almacenamiento y seguridad.

## Ver también

- [Documentación mktemp](https://fishshell.com/docs/current/cmds/mktemp.html)
- [Documentación touch](https://fishshell.com/docs/current/cmds/touch.html)