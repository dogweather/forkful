---
title:                "Python: Leyendo un archivo de texto"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Una de las tareas más comunes en programación es leer archivos de texto. Ya sea para procesar datos, extraer información o simplemente leer un archivo de configuración, saber cómo leer un archivo de texto en Python es una habilidad esencial para cualquier programador.

## Cómo

Para leer un archivo de texto en Python, se utiliza la función `open()` y el método `read()`. Por ejemplo, si tenemos un archivo llamado "datos.txt" con el siguiente contenido:

```
1,2,3
4,5,6
7,8,9
```

Podemos leerlo en Python de la siguiente manera:

```python
archivo = open("datos.txt", "r") # Se abre el archivo en modo lectura
contenido = archivo.read() # Se lee el contenido del archivo
print(contenido) # Se imprime el contenido
```

La salida de este código sería:

```
1,2,3
4,5,6
7,8,9
```

El archivo se abre en modo lectura (`"r"`), pero también se puede abrir en modo escritura (`"w"`) o modo agregado (`"a"`) dependiendo de lo que se desee hacer con el archivo.

También es posible leer el archivo línea por línea utilizando el método `readline()`:

```python
archivo = open("datos.txt", "r")
linea1 = archivo.readline() # Se lee la primera línea
linea2 = archivo.readline() # Se lee la segunda línea
print(linea1) # Se imprime la primera línea
print(linea2) # Se imprime la segunda línea
```

Otra forma de leer un archivo de texto es utilizar un bucle `for` para iterar sobre cada línea:

```python
archivo = open("datos.txt", "r")
for linea in archivo:
    print(linea) # Se imprime cada línea
```

## Deep Dive

Además de los métodos `read()` y `readline()`, también existen otros métodos útiles para leer archivos de texto en Python, como por ejemplo `readlines()`, que devuelve una lista con todas las líneas del archivo, o `readline(n)`, que lee únicamente los primeros `n` caracteres de cada línea.

También es importante tener en cuenta que, al leer un archivo de texto, se utiliza el carácter de nueva línea (`\n`) para separar cada línea. Por lo tanto, si se desea manipular los datos del archivo, es importante eliminar ese carácter o convertirlo a otro formato antes de trabajar con ellos.

## Ver También

- [Documentación oficial de Python sobre lectura de archivos de texto] (https://docs.python.org/es/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Artículo sobre lectura de archivos de texto en GeeksforGeeks] (https://www.geeksforgeeks.org/reading-writing-text-files-python/)
- [Tutoriales de lectura de archivos de texto en Programiz] (https://www.programiz.com/python-programming/file-operation)