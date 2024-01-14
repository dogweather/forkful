---
title:    "Python: Comprobando si existe un directorio"
keywords: ["Python"]
---

{{< edit_this_page >}}

#¿Por qué deberías verificar si un directorio existe? 

Verificar si un directorio existe puede ser una tarea importante al trabajar con archivos en un proyecto de programación. Esta acción te ayudará a evitar errores y a tomar decisiones en tu código en caso de que el directorio no se encuentre en tu sistema.

## Cómo hacerlo

Para verificar si un directorio existe en Python, podemos utilizar la función `os.path.exists()` que nos permitirá evaluar si un camino o ruta especificada existe. El siguiente código muestra cómo podemos utilizar esta función en un programa simple:

```Python
import os

ruta = "C:/proyecto/archivos"

if os.path.exists(ruta):
    print("El directorio existe")
else:
    print("El directorio no existe")
```

Si el directorio especificado existe, se mostrará el mensaje "El directorio existe". De lo contrario, se imprimirá "El directorio no existe".

## Profundizando más

Si deseas obtener más información acerca de un directorio específico, también puedes utilizar la función `os.path.isdir()` que te permitirá verificar si la ruta especificada es un directorio.

```Python
import os

ruta = "C:/proyecto/archivos"

if os.path.isfile(ruta):
    print("La ruta especificada es un archivo")
elif os.path.isdir(ruta):
    print("La ruta especificada es un directorio")
else:
    print("La ruta especificada no existe")
```

Además, si por alguna razón necesitas crear un nuevo directorio en tu sistema, puedes hacerlo utilizando la función `os.mkdir()` y especificando la ruta del nuevo directorio.

```Python
import os

ruta = "C:/proyecto/nuevo_directorio"

os.mkdir(ruta) #crea el directorio en la ruta especificada
print("El nuevo directorio ha sido creado con éxito")
```

Recuerda que es una buena práctica utilizar manejo de excepciones al trabajar con archivos y directorios, ya que pueden ocurrir errores inesperados al interactuar con el sistema de archivos.

## Ver también

- Documentación oficial de Python para el módulo `os.path`: https://docs.python.org/es/3/library/os.path.html
- Tutorial de Real Python sobre manejo de archivos y directorios en Python: https://realpython.com/working-with-files-in-python/
- Tutorial de Programiz sobre operaciones de archivos y directorios en Python: https://www.programiz.com/python-programming/file-operation