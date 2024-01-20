---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Crear un archivo temporal en Python es cuando generamos un archivo que existe hasta que se cierre el programa o se borre explícitamente. Los programadores crean archivos temporales principalmente por dos razones: para almacenar grandes cantidades de datos que no se ajustan a la memoria y para compartir datos entre procesos.

## ¿Cómo hacerlo?

Python proporciona un módulo `tempfile` que nos permite trabajar con archivos temporales. Aquí hay un ejemplo de cómo podemos crear, escribir y leer un archivo temporal:

```Python
import tempfile

# Crear un archivo temporal
temp = tempfile.TemporaryFile()

# Escribir en el archivo temporal
temp.write(b'Python es genial!')

# Ir al inicio del archivo
temp.seek(0)

# Leer el archivo
print(temp.read())  # Output: b'Python es genial!'

# Cerrar y borrar el archivo
temp.close()
```

## Inmersión Profunda

La generación de archivos temporales ha sido una práctica común en la programación desde los primeros días de las computadoras. Antes de la aparición de la memoria masiva como la tenemos en la actualidad, los archivos temporales eran una necesidad absoluta.

Alternativamente, puedes usar `NamedTemporaryFile` si deseas un archivo con un nombre que puedes pasar a otras funciones. Hay que tener en cuenta que estos archivos tienen un plazo de existencia más largo que `TemporaryFile` y sólo se borran cuando son cerrados explícitamente.

El módulo `tempfile` en Python usa una rutina subyacente segura para generar nombres de archivos únicos, las funciones de este módulo no pueden ser utilizadas de manera segura para nombres de archivos proporcionados por el usuario antes de abrirlos.

## Ver También

* [Documentación oficial de Python para el módulo tempfile](https://docs.python.org/3/library/tempfile.html)
* [Tutorial de Python sobre manejo de archivos](https://realpython.com/read-write-files-python/)