---
title:    "Python: Creando un archivo temporal"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Por qué crear un archivo temporal en Python

Crear un archivo temporal en Python puede ser muy útil en ciertas situaciones, como por ejemplo cuando necesitamos almacenar datos temporales que no necesitamos conservar después del uso. También puede ser útil para almacenar datos sensibles que no queremos que permanezcan en nuestro sistema permanentemente.

## Cómo crear un archivo temporal en Python

Para crear un archivo temporal en Python, utilizaremos el módulo `tempfile`. Este módulo nos permite crear y manipular archivos temporales de manera sencilla. A continuación, un ejemplo de cómo crear un archivo temporal con contenido utilizando `tempfile`:

```Python
import tempfile

with tempfile.NamedTemporaryFile() as temp:
    # Escribimos contenido en el archivo temporal
    temp.write("¡Hola mundo!")

    # Leemos el contenido del archivo temporal
    temp.seek(0)
    contenido = temp.read()

# El archivo temporal se elimina automáticamente una vez que salimos del "with"
```
El resultado de este código sería un archivo temporal con el texto "¡Hola mundo!" escrito dentro de él.

## Profundizando en la creación de archivos temporales en Python

Cuando creamos un archivo temporal, podemos utilizar diferentes funciones y métodos del módulo `tempfile` para personalizar su comportamiento. Algunas opciones incluyen:

- Asignar un nombre específico al archivo temporal utilizando `tempfile.NamedTemporaryFile()`
- Especificar el directorio donde se creará el archivo temporal utilizando el argumento `dir` en `tempfile.NamedTemporaryFile()`
- Crear archivos temporales con una extensión específica utilizando `tempfile.NamedTemporaryFile(suffix=".txt")`

Además, es importante tener en cuenta que al utilizar `tempfile` estamos creando un archivo temporal en nuestra máquina local. Si necesitamos compartir este archivo entre diferentes máquinas o sistemas, debemos utilizar `tempfile.TemporaryFile(dir="ruta/del/directorio")` para especificar una ubicación compartida en la red.

# Ver también

- Documentación oficial de `tempfile`: https://docs.python.org/es/3/library/tempfile.html
- Cómo trabajar con archivos en Python: https://realpython.com/read-write-files-python/
- Ejemplo de uso de `tempfile` en un proyecto: https://www.blog.pythonlibrary.org/2014/02/27/python-101-an-intro-to-tempfile/