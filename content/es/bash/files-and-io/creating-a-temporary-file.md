---
title:                "Creando un archivo temporal"
aliases:
- /es/bash/creating-a-temporary-file/
date:                  2024-01-20T17:39:25.826513-07:00
model:                 gpt-4-1106-preview
simple_title:         "Creando un archivo temporal"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Crear un fichero temporal es como hacer una nota adhesiva digital, algo temporal para guardar datos que no necesitas a largo plazo. Los programadores lo hacen para manejar datos transitorios, como almacenamiento intermedio al procesar información o para evitar bloqueos de recursos.

## Cómo Hacerlo:
```Bash
# Crear un archivo temporal con mktemp
tempfile=$(mktemp)

# Comprobar que se ha creado
echo "Archivo temporal creado en: $tempfile"

# Usar el archivo para algo, por ejemplo, guardar la lista de directorios
ls -la > "$tempfile"

# Ver el contenido del archivo temporal
cat "$tempfile"

# Eliminar el archivo temporal cuando terminas
rm "$tempfile"
```

Output esperado (el nombre del archivo temporal variará):
```
Archivo temporal creado en: /tmp/tmp.Iy5NVxG2Gp
total 28
drwxr-xr-x 2 usuario grupo 4096 Mar 25 09:30 .
drwxrwxrwt 9 root    root  4096 Mar 25 09:30 ..
-rw-r--r-- 1 usuario grupo    0 Mar 25 09:30 archivo.jpg
```

## Análisis a Fondo:
Históricamente, los ficheros temporales se han usado para evitar que la memoria se llene con datos que solo son relevantes durante ese instante de ejecución. Existen diversas formas de crearlos: `mktemp` es seguro porque asegura que el nombre del archivo sea único, evitando conflictos. Alternativamente, otros programadores podrían usar `$$` (el ID del proceso) para un nombre semi-único, algo así como `/tmp/tempfile.$$`, pero esto no es recomendable porque no garantiza la unicidad y es más vulnerable a colisiones. En los sistemas basados en Unix, el directorio `/tmp` es un lugar común para estos archivos, ya que se limpia en cada reinicio.

## Vea También:
- GNU Coreutils mktemp: https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html
- Bash Reference Manual (Filesystem Hierarchy Standard): https://www.gnu.org/software/bash/manual/html_node/The-Shopt-Builtin.html
- Advanced Bash-Scripting Guide: Temporary Files: https://tldp.org/LDP/abs/html/tempfiles.html
