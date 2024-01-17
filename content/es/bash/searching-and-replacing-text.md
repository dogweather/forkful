---
title:                "Buscando y reemplazando texto"
html_title:           "Bash: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Buscar y reemplazar texto es una tarea común en la programación. Se trata de encontrar un cierto texto en un archivo o documento y reemplazarlo por otro texto diferente. Los programadores suelen hacer esto para corregir errores, actualizar información o automatizar tareas repetitivas.

## Cómo:

Para buscar y reemplazar texto en Bash, podemos usar el comando `sed`. Por ejemplo, si queremos reemplazar todas las apariciones de la palabra "perro" por "gato" en un archivo llamado "animales.txt", podemos usar el siguiente comando:

```
sed -i 's/perro/gato/g' animales.txt
```

En este comando, la opción `-i` asegura que los cambios se realicen directamente en el archivo y `s/perro/gato/g` es la expresión regular que especifica qué se debe buscar y reemplazar.

El comando `sed` también nos permite buscar y reemplazar texto en múltiples archivos a la vez. Por ejemplo, si queremos reemplazar "miércoles" por "jueves" en todos los archivos con extensión ".txt", podemos usar:

```
sed -i 's/miércoles/jueves/g' *.txt
```

## Profundizando:

El comando `sed` tiene sus orígenes en el sistema operativo Unix en la década de 1970 y es utilizado ampliamente en la línea de comandos para editar archivos de texto. Sin embargo, también hay otras herramientas disponibles para esta tarea, como `awk` y `perl`.

Además, también existen editores de texto con capacidades avanzadas de búsqueda y reemplazo, como Vim o Emacs.

Para aprender más sobre el uso de expresiones regulares en Bash, puedes consultar la documentación oficial de `sed` y otros recursos en línea.

## Ver también:

- Documentación oficial de `sed`: https://www.gnu.org/software/sed/
- Expresiones regulares en Bash: https://www.linuxtopia.org/online_books/advanced_bash_scripting_guide/x22692.html