---
title:    "Python: Leyendo un archivo de texto"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por qué

Leer un archivo de texto es una habilidad fundamental en programación de computadoras. Te permite acceder y manipular información almacenada en un archivo de texto, lo cual es útil para una amplia variedad de aplicaciones. En este post, aprenderemos cómo leer un archivo de texto en Python y profundizaremos en los detalles de esta operación.

## Cómo hacerlo

Para leer un archivo de texto en Python, primero necesitamos crear un objeto de archivo utilizando la función `open()`. Esta función toma dos argumentos: el nombre del archivo y el modo de acceso. El modo de acceso determina si queremos leer, escribir o agregar contenido al archivo.

Luego, podemos usar el método `read()` en el objeto de archivo para leer el contenido del archivo entero. También podemos usar el método `readline()` para leer línea por línea, o `readlines()` para obtener una lista con todas las líneas del archivo.

Veamos un ejemplo de cómo leer un archivo de texto llamado "ejemplo.txt" que contiene los siguientes datos:

```
Hola, esto es un ejemplo.
Espero que te sea útil.
Saludos.
```

```Python
archivo = open("ejemplo.txt", "r") # Abrir el archivo en modo de lectura
contenido = archivo.read() # Leer todo el contenido del archivo
print(contenido) # Imprimir el contenido en la consola
archivo.close() # Cerrar el archivo
```

**Salida:**
```
Hola, esto es un ejemplo.
Espero que te sea útil.
Saludos.
```

## Profundizando en la lectura de archivos de texto

Además de los métodos mencionados anteriormente, existen otras formas de leer un archivo de texto con Python. Por ejemplo, podemos usar la función `with` para manejar la apertura y cierre del archivo automáticamente, lo cual es útil para evitar errores en caso de que olvidemos cerrar el archivo.

También podemos especificar el tamaño en bytes que queremos leer con el método `read()` agregando el número de bytes como argumento. Y si queremos leer un archivo en un idioma diferente al inglés, podemos utilizar el parámetro `encoding` en la función `open()` para especificar la codificación del archivo.

Por último, es importante mencionar que debemos tener cuidado con leer archivos grandes, ya que esto puede afectar el rendimiento de nuestro programa. En lugar de leer todo el contenido de una vez, podemos optar por leerlo en bloques más pequeños para mantener un buen rendimiento.

## Ver También

- [Documentación oficial de Python sobre lectura de archivos](https://docs.python.org/es/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorial de Real Python sobre lectura y escritura de archivos](https://realpython.com/read-write-files-python/)
- [Artículo de GeeksforGeeks sobre manejo de archivos en Python](https://www.geeksforgeeks.org/reading-and-writing-files-in-python/)