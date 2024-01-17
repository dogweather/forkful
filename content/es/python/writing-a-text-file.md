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

## ¿Qué y por qué?

Escribir un archivo de texto es simplemente guardar información en un archivo legible por el programa. Es una forma práctica de almacenar datos para que puedan ser utilizados por programas de computadora. Los programadores utilizan textos para crear bases de datos, generar informes y automatizar tareas.

## ¿Cómo hacerlo?

Para escribir un archivo de texto en Python, primero necesitamos abrir el archivo utilizando la función open() y especificar la ubicación y el nombre del archivo. Luego, utilizamos el método write() para agregar contenido al archivo. Finalmente, cerramos el archivo con la función close() para asegurarnos de que todos los cambios se guarden correctamente.

```Python
archivo = open("mi_archivo.txt", "w") # Abrir un archivo en modo escritura
archivo.write("¡Hola, mundo!") # Agregar contenido al archivo
archivo.close() # Cerrar el archivo
```

Si deseamos escribir en un archivo existente sin sobrescribir su contenido, podemos abrirlo en modo de agregado ("a") en lugar de modo escritura ("w").

## Profundizando

Escribir archivos de texto ha sido una tarea común en la programación de computadoras desde los inicios de la informática. Antes, los lenguajes de programación requerían que se escribieran códigos específicos para manejar archivos de texto, pero con el tiempo, se han desarrollado librerías y módulos que facilitan esta tarea. Algunas alternativas para escribir archivos de texto en Python son los módulos csv y shutil, que ofrecen funciones específicas para la manipulación de archivos de texto.

También es importante mencionar que existen diferentes modos de apertura de archivos, como leer ("r"), añadir ("a") y binario ("b"). Es importante seleccionar el modo adecuado para la tarea que estamos realizando.

## Véase también

Documentación oficial de Python sobre escritura de archivos: https://docs.python.org/es/3/tutorial/inputoutput.html#reading-and-writing-files

Tutorial en español sobre escritura de archivos en Python: https://recursospython.com/guias-y-manuales/escribir-archivos-con-python/