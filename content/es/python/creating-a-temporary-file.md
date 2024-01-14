---
title:                "Python: Creando un archivo temporal"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Por qué crear archivos temporales en Python

Crear archivos temporales en Python puede ser útil en situaciones donde necesitas almacenar información de forma temporal y luego eliminarla. Esto puede ser especialmente útil en programas que manejan grandes cantidades de datos o en scripts que requieren manipular y guardar información de forma dinámica.

## Cómo crear un archivo temporal en Python

Para crear un archivo temporal en Python, puedes utilizar la biblioteca `tempfile`. Esta biblioteca proporciona varias funciones para gestionar archivos temporales, como `TemporaryFile()` y `NamedTemporaryFile()`. Aquí hay un ejemplo de cómo crear y escribir en un archivo temporal:

```Python
import tempfile

# Crear un archivo temporal utilizando la función TemporaryFile()
with tempfile.TemporaryFile() as tmp_file:
    # Escribir una línea en el archivo temporal
    tmp_file.write(b'Este es un archivo temporal.')

    # Regresar al inicio del archivo
    tmp_file.seek(0)

    # Leer y imprimir el contenido del archivo
    print(tmp_file.read().decode('utf-8'))

# El archivo temporal se elimina automáticamente al salir del bloque "with"
```

La salida de este código debería ser `Este es un archivo temporal.`

También es posible nombrar el archivo temporal utilizando la función `NamedTemporaryFile()`. Esto crea un archivo temporal con el nombre especificado y en el directorio temporal por defecto. A continuación, se muestra un ejemplo de cómo crear un archivo temporal con un nombre personalizado:

```Python
import tempfile

# Crear un archivo temporal con un nombre personalizado.
# El archivo se elimina automáticamente al salir del bloque "with".
with tempfile.NamedTemporaryFile(suffix='.txt', delete=False) as tmp_file:
    # Escribir varias líneas en el archivo temporal
    lines = ['Este es un archivo', 'temporal', 'con varias líneas.']
    tmp_file.writelines(line.encode('utf-8') for line in lines)

    # Imprimir la ruta del archivo temporal
    print(tmp_file.name)

# El archivo se eliminará automáticamente al salir del bloque "with" y no estará disponible en la ubicación especificada.
```

## Profundizando en la creación de archivos temporales

Cuando se crea un archivo temporal utilizando la función `NamedTemporaryFile()`, este se almacena en el directorio temporal de tu sistema operativo. Puedes obtener la ruta de este directorio utilizando la función `gettempdir()` de la biblioteca `tempfile`.

Además, puedes especificar el modo de apertura del archivo temporal utilizando el argumento `mode` en la función `NamedTemporaryFile()`. Por ejemplo, si deseas crear un archivo temporal en modo de sólo lectura, puedes usar `mode='r'`.

También es importante tener en cuenta que, mientras el archivo temporal esté abierto, otros programas no podrán acceder a él. Por lo tanto, es importante cerrar el archivo o salir del bloque `with` una vez que hayas terminado de trabajar con él.

# Ver también

- Documentación oficial de la biblioteca `tempfile`: https://docs.python.org/es/3/library/tempfile.html
- Tutorial sobre gestión de archivos temporales en Python: https://realpython.com/python-tempfile/
- Ejemplos prácticos de uso de archivos temporales en Python: https://www.programcreek.com/python/example/268/tempfile.NamedTemporaryFile