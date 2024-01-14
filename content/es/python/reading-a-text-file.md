---
title:    "Python: Lectura de un archivo de texto"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué leer un archivo de texto en Python?

Si estás aprendiendo a programar en Python o si ya tienes experiencia, es importante entender cómo trabajar con archivos de texto. Esto te permitirá manipular y analizar datos de una manera más eficiente y práctica. En este post, te mostraremos cómo leer un archivo de texto en Python y profundizaremos en el proceso detrás de esto.

## Cómo hacerlo

Primero, debemos abrir el archivo de texto utilizando la función `open()`. Esta función recibe dos argumentos: el nombre del archivo y el modo en el que queremos abrirlo (lectura, escritura, etc.). Por ejemplo, si queremos abrir un archivo llamado "texto.txt" en modo de lectura, escribiríamos lo siguiente en nuestro código:

```Python
archivo = open("texto.txt", "r")
```

Una vez abierto el archivo, podemos leer su contenido utilizando la función `read()`. Esta función devuelve todo el contenido del archivo como una sola cadena de texto. Por ejemplo:

```Python
contenido = archivo.read()
print(contenido)
```

El resultado de este código sería la impresión de todo el contenido del archivo en la consola.

Si queremos leer el archivo línea por línea, podemos utilizar la función `readlines()`. Esta función devuelve una lista de cadenas, donde cada elemento es una línea del archivo. Por ejemplo:

```Python
lineas = archivo.readlines()
print(lineas)
```

Si queremos trabajar con el contenido del archivo como una lista, podemos utilizar la función `readlines()` y luego iterar sobre cada elemento de la lista utilizando un ciclo `for`.

## Profundizando

Para entender mejor cómo funciona el proceso de lectura de un archivo de texto en Python, es importante conocer cómo se almacenan los datos en el mismo. En términos simples, un archivo de texto es una secuencia de bytes, donde cada byte representa un carácter.

Cuando abrimos un archivo de texto en Python y utilizamos la función `read()`, lo que sucede detrás de escena es que se lee la cantidad de bytes especificada y se los convierte a caracteres. Esto significa que si nuestro archivo contiene caracteres especiales, como por ejemplo letras con acentos o símbolos, estos serán representados correctamente.

Además de leer archivos de texto, en Python también tenemos la opción de escribir en ellos utilizando la función `write()`. Esto nos permite guardar información o resultados de nuestros programas en un archivo de texto para utilizarlos posteriormente.

## Ver también

- Documentación oficial de Python para trabajar con archivos: https://docs.python.org/es/3/tutorial/inputoutput.html#reading-and-writing-files
- Tutorial sobre lectura y escritura de archivos en Python: https://realpython.com/read-write-files-python/
- Ejemplos de código para leer y escribir archivos en Python: https://www.programiz.com/python-programming/file-operation