---
title:                "Comprobando si existe un directorio"
html_title:           "Python: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Antes de aprender cómo verificar si un directorio existe en Python, es importante entender por qué esto puede ser útil. En programación, a menudo trabajamos con diferentes archivos y directorios, y es posible que necesitemos asegurarnos de que un directorio en particular exista antes de realizar ciertas operaciones con él. Al verificar si un directorio existe, podemos evitar errores y asegurarnos de que nuestro código funcione correctamente.

## Cómo hacerlo

Para verificar si un directorio existe en Python, podemos utilizar la función `path.exists()` del módulo `os`. Veamos un ejemplo de cómo hacerlo:

```python
import os

# Definimos el directorio que queremos verificar
directorio = "/home/usuario/documentos"

# Verificamos si existe
if os.path.exists(directorio):
    print("El directorio existe.")
else:
    print("El directorio no existe.")
```

En este ejemplo, utilizamos la función `path.exists()` para verificar si el directorio `documentos` existe dentro de la ruta `/home/usuario`. Si el directorio existe, se imprimirá "El directorio existe". Si no existe, se imprimirá "El directorio no existe".

También podemos utilizar la función `path.isdir()` del módulo `os` para verificar si un directorio es válido:

```python
import os

# Definimos el directorio que queremos verificar
directorio = "/home/usuario/documentos"

# Verificamos si es un directorio válido
if os.path.isdir(directorio):
    print("Es un directorio válido.")
else:
    print("No es un directorio válido.")
```

En este caso, si el directorio es válido, se imprimirá "Es un directorio válido". Si no lo es, se imprimirá "No es un directorio válido".

## Profundizando

En Python, podemos utilizar la función `os.path.exists()` para verificar si un directorio existe en cualquier ubicación del sistema operativo, ya sea en un directorio local o incluso en una ubicación en línea.

También es importante tener en cuenta que al verificar si un directorio existe, no solo podemos usar una variable que contenga la ruta completa del directorio, sino también partes de la ruta. Por ejemplo, en lugar de escribir `/home/usuario/documentos`, podríamos escribir simplemente `documentos` y Python lo reconocerá siempre y cuando se encuentre dentro del directorio de trabajo actual.

## Ver también

- [Documentación oficial de Python sobre la función `os.path.exists()`](https://docs.python.org/es/3/library/os.path.html#os.path.exists)
- [Ejemplo de uso de `os.path.exists()` en el blog Real Python](https://realpython.com/python-pathlib/)