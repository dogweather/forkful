---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Creación de Archivos Temporales en Bash

## ¿Qué & Por Qué?

Crear un archivo temporal en Bash implica generar un archivo que almacena datos de forma temporal. Esta práctica ayuda a los programadores a guardar datos volátiles que no necesitan persistir.

## Cómo Hacerlo:

Bash nos ofrece`mktemp`, una función incorporada, para crear archivos temporales fácilmente. 

```Bash
# Crear un archivo temporal
tempFile=$(mktemp)

# Usar el archivo temporal
echo "Este es un archivo temporal" > $tempFile
cat $tempFile

# Borrar el archivo temporal
rm $tempFile
```

Este código genera un archivo temporal vacío, inserta un texto, lo muestra y después lo borra.

## Análisis Profundo

El comando `mktemp` se introdujo en la versión 8.21 de GNU Coreutils, aportando una forma segura de crear archivos temporales en scripts de Bash. Anteriormente, los programadores a veces usaban comandos como `$$` para generar nombres de archivos temporales, pero esto podría ser problemático debido a la posibilidad de colisiones de nombres.

Una alternativa a `mktemp` es utilizar `mktemp -d` para crear un directorio temporal en lugar de un archivo temporal. 

La implementación de `mktemp` en Bash genera un archivo con un nombre único en el directorio `/tmp `. De todos modos, puedes pasar la ruta absoluta como parámetro en caso de que desees que el archivo se cree en otro directorio.

## Ver También

Para más información, consulta los siguientes enlaces:

- `man mktemp`: https://man7.org/linux/man-pages/man1/mktemp.1.html
- Creación segura de archivos temporales en Shell Scripts: https://www.cyberciti.biz/faq/unix-how-to-create-temporary-random-file-names-shell-script/