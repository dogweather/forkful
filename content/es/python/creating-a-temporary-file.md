---
title:                "Creando un archivo temporal"
html_title:           "Python: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Por qué crear un archivo temporal en Python?

Crear un archivo temporal en Python puede ser útil cuando necesitamos almacenar datos temporales en nuestro programa. Por ejemplo, si queremos guardar información mientras se ejecuta un proceso, pero no deseamos crear un archivo permanente en el sistema, un archivo temporal es la mejor opción.

## Cómo crear un archivo temporal en Python

Crear un archivo temporal en Python es muy sencillo. Solo necesitamos importar el módulo `tempfile` y utilizar la función `NamedTemporaryFile()`, como se muestra a continuación:

```Python
import tempfile

archivo_temporal = tempfile.NamedTemporaryFile()
```

Podemos especificar el nombre y la extensión del archivo temporal si lo deseamos, como en el siguiente ejemplo:

```Python
import tempfile

archivo_temporal = tempfile.NamedTemporaryFile(suffix='.txt', prefix='datos_')
```

Para escribir en el archivo temporal, podemos utilizar métodos similares a los de cualquier archivo en Python, como `write()` o `writelines()`. Una vez que hayamos terminado de utilizar el archivo, podemos cerrarlo con el método `close()`. El archivo temporal se borrará automáticamente al cerrarlo.

## Una mirada más profunda: ¿cómo funciona un archivo temporal en Python?

Un archivo temporal en Python se crea en la ubicación especificada por el sistema operativo, generalmente en la carpeta de archivos temporales. Es importante tener en cuenta que el archivo se borra automáticamente al cerrarlo, pero también es posible especificar que se conserve en el sistema después de su uso. Esto se puede hacer utilizando el argumento `delete=False` al crear el archivo.

Además, un archivo temporal se crea en modo binario por defecto. Si deseamos abrirlo en modo texto, podemos especificarlo con el argumento `mode='w'` al crearlo.

## Ver también

- Documentación oficial de `tempfile` en la biblioteca estándar de Python: https://docs.python.org/es/3/library/tempfile.html
- Tutorial sobre el manejo de archivos en Python: https://realpython.com/read-write-files-python/
- Ejemplos prácticos de uso de archivos temporales: https://stackabuse.com/managing-temporary-files-in-python/