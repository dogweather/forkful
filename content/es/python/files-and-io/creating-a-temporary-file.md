---
date: 2024-01-20 17:41:07.600063-07:00
description: "Crear archivos temporales es pr\xE1ctica com\xFAn en programaci\xF3\
  n para guardar datos que solo necesitas durante la ejecuci\xF3n de tu script. Se\
  \ hace para no dejar\u2026"
lastmod: '2024-03-13T22:44:58.633522-06:00'
model: gpt-4-1106-preview
summary: "Crear archivos temporales es pr\xE1ctica com\xFAn en programaci\xF3n para\
  \ guardar datos que solo necesitas durante la ejecuci\xF3n de tu script. Se hace\
  \ para no dejar\u2026"
title: Creando un archivo temporal
---

{{< edit_this_page >}}

## Qué y Por Qué?
Crear archivos temporales es práctica común en programación para guardar datos que solo necesitas durante la ejecución de tu script. Se hace para no dejar huellas innecesarias en el disco duro y para manejar información sensible que no queremos que perdure.

## Cómo hacerlo:
Para trabajar con archivos temporales en Python, usamos el módulo `tempfile`. Aquí un ejemplo simple:

```Python
import tempfile

# Crear un archivo temporal
with tempfile.TemporaryFile(mode='w+t') as tmp:
    # Escribir algo en el archivo temporal
    tmp.write('Hola, este es un archivo temporal!\n')
    tmp.write('Adiós!')

    # Retroceder al inicio del archivo y leer lo que escribimos
    tmp.seek(0)
    print(tmp.read())

# El archivo se ha borrado automáticamente.
```

Si corres ese script, verás el contenido del archivo temporal, pero una vez que finalice, el archivo desaparecerá.

## Deep Dive

Los archivos temporales no son un invento moderno. Desde los albores de la informática, manejar datos que no necesitaban persistir llevó a su creación. En sistemas Unix y similares, `/tmp` es un directorio típico para estos menesteres.

Alternativamente, el módulo `tempfile` ofrece diferentes maneras de crear archivos y directorios temporales, como `NamedTemporaryFile` que te da un archivo con nombre el cual es útil si necesitas una referencia en el sistema de archivos, y `mkdtemp` para crear directorios temporales.

Bajo el capó, `tempfile` se comunica con el sistema operativo para asegurarse de que estos archivos se guarden en lugares adecuados y se manejen de forma segura (e.g., con permisos apropiados).

## Ver También

Aquí algunos enlaces útiles para profundizar:

- [Documentación del módulo tempfile de Python](https://docs.python.org/3/library/tempfile.html)
- [Una guía de los sistemas de archivos en Unix](https://tldp.org/LDP/intro-linux/html/sect_03_01.html)
- [Consejos de seguridad para archivos temporales en Unix](https://www.netmeister.org/blog/mktemp.html)

Estos recursos te ayudarán a obtener un mejor entendimiento del trabajo con archivos y directorios temporales en tus proyectos de Python.
