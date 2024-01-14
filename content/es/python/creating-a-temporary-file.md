---
title:    "Python: Creando un archivo temporal"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Crear un archivo temporal es una herramienta útil en la programación de Python para realizar tareas específicas, como almacenar datos temporales, realizar pruebas o ejecutar scripts de forma segura.

## Cómo

```Python
import tempfile

#Crear un archivo temporal
temp_file = tempfile.NamedTemporaryFile()

#Escribir datos en el archivo
temp_file.write(b"Hola, ¿cómo estás?")
temp_file.seek(0)

#Leer y mostrar los datos del archivo
print(temp_file.read())

#Cerrar y eliminar el archivo temporal
temp_file.close()
```

**Salida:**

```
b'Hola, ¿cómo estás?'
```

## Profundizando

Un archivo temporal se crea mediante la función `NamedTemporaryFile()` del módulo `tempfile`, que toma dos argumentos opcionales `mode` (modo de apertura del archivo) y `delete` (que indica si se debe eliminar el archivo temporal automáticamente al cerrarlo).

Al utilizar el método `write()` y luego `seek()` para mover el cursor al inicio del archivo, podemos escribir y leer datos en el archivo temporal creado. Al cerrar el archivo con el método `close()`, se elimina automáticamente.

Otra forma de crear un archivo temporal es utilizando el administrador de contextos `with`, lo que garantiza que el archivo se elimine al salir del bloque `with`:

```Python
with tempfile.NamedTemporaryFile() as temp_file:
    #Escribir datos en el archivo
    temp_file.write(b"Hola, ¿cómo estás?")
    temp_file.seek(0)

    #Leer y mostrar los datos del archivo
    print(temp_file.read())

#El archivo se elimina automáticamente al salir del bloque with
```

## Ver también

- [Documentación oficial de Python sobre el módulo tempfile](https://docs.python.org/es/3/library/tempfile.html)
- [Tutorial de Real Python sobre el uso de archivos temporales en Python](https://realpython.com/python-tempfile/)
- [Artículo de Programa Ahora sobre la importancia de los archivos temporales en la programación](https://programandoahora.com.ar/por-que-usar-archivos-temporales/)