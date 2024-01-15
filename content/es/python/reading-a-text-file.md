---
title:                "Leyendo un archivo de texto"
html_title:           "Python: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto?

Leer un archivo de texto es una tarea común en la programación. Puede ser necesario para procesar datos, almacenar información o simplemente para leer un documento. Es esencial tener un buen conocimiento de cómo leer un archivo de texto en Python para poder manipularlo según sea necesario.

## Cómo hacerlo

En Python, podemos leer un archivo de texto utilizando la función `open()` y el modo `read`. También podemos especificar la codificación según sea necesario.

```Python
with open("archivo.txt", mode="r", encoding="utf-8") as file:
    contenido = file.read()
    print(contenido)
```

En este ejemplo, utilizamos la palabra clave `with` para asegurarnos de que el archivo se cierre automáticamente después de su uso. Utilizamos `mode="r"` para indicar que queremos leer el archivo y `encoding="utf-8"` para especificar la codificación. Luego, utilizamos el método `read()` para leer todo el contenido del archivo y lo imprimimos en la consola.

Alternativamente, también podemos utilizar un bucle `for` para leer línea por línea del archivo.

```Python
with open("archivo.txt", mode="r", encoding="utf-8") as file:
    for linea in file:
        print(linea)
```

En este caso, el archivo se cierra automáticamente después de que se recorran todas las líneas.

## Profundizando

Además de leer todo el contenido de un archivo de texto, también podemos utilizar otros métodos para leer solo partes específicas, como `readline()` para leer una línea a la vez o `readlines()` para leer todas las líneas y almacenarlas en una lista.

También es importante tener en cuenta que siempre debemos cerrar un archivo después de leerlo para liberar recursos. Esto se puede hacer explícitamente con el método `close()` o utilizando la palabra clave `with` como se mostró en los ejemplos anteriores.

## Ver también

- [Documentación oficial de Python sobre lectura y escritura de archivos](https://docs.python.org/es/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorial de Programiz sobre lectura y escritura de archivos en Python](https://www.programiz.com/python-programming/file-operation)