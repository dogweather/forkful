---
title:                "Escribiendo un archivo de texto"
html_title:           "Python: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Una de las tareas más comunes en la programación es la lectura y escritura de archivos de texto. Estos archivos son utilizados para guardar información que necesita ser accesada y modificada por el programa. Saber cómo escribir un archivo de texto es una habilidad esencial para cualquier programador.

## ¿Cómo hacerlo?

Escribir un archivo de texto en Python es un proceso sencillo que requiere solo unos pocos pasos.

1. Primero, necesitamos crear y abrir el archivo en el que queremos escribir. Esto se puede hacer utilizando la función `open()` y especificando el modo de escritura (`w`).

```
archivo = open("mi_archivo.txt", "w")
```

2. Ahora podemos escribir en el archivo utilizando el método `write()`. Este método toma una cadena como argumento y la escribe en el archivo.

```
archivo.write("¡Hola! Este es un archivo de texto escrito en Python.")
```

3. No olvidemos cerrar el archivo después de terminar de escribir. Esto se hace utilizando el método `close()`.

```
archivo.close()
```

También podemos escribir varias líneas en el archivo utilizando un bucle `for` y una lista de cadenas.

```
lineas = ["Esta es la primera línea.", "Esta es la segunda línea.", "Esta es la tercera línea."]

for linea in lineas:
    archivo.write(linea + "\n")
```

¡Y eso es todo! Ahora tendremos un archivo de texto con las líneas que especificamos.

## Profundizando

Hay algunos detalles importantes a tener en cuenta cuando se escribe un archivo de texto en Python. Por ejemplo, si el archivo que estamos abriendo ya existe, el modo `w` lo sobreescribirá completamente. Si queremos agregar texto al final del archivo, podemos utilizar el modo `a` (append).

También es importante asegurarse de cerrar el archivo después de usarlo para evitar problemas de memoria.

## Ver también

- [Documentación oficial de Python sobre escritura de archivos](https://docs.python.org/es/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorial de Real Python sobre escritura de archivos](https://realpython.com/read-write-files-python/)
- [Video tutorial de Programiz sobre escritura de archivos en Python](https://www.programiz.com/python-programming/file-operation#write)