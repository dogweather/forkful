---
title:                "Escribiendo un archivo de texto"
html_title:           "C: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Escribir un archivo de texto es una forma de almacenar información en un formato legible para los humanos. Los programadores suelen hacerlo porque es útil para guardar datos o configuraciones que pueden ser leídas y modificadas fácilmente.

## Cómo:

Para escribir un archivo de texto en C, primero necesitas abrirlo con la función `fopen()`, especificando el nombre del archivo y el modo en el que deseas abrirlo. Por ejemplo, para escribir en un archivo llamado "datos.txt", puedes usar el modo "w" que significa "write" o escritura.

```C
// Abrir el archivo "datos.txt" en modo escritura
FILE *archivo = fopen("datos.txt", "w");
```

Luego, puedes utilizar la función `fprintf()` para escribir en el archivo. En el siguiente ejemplo, se escribe una línea con el texto "¡Hola mundo!" y un número entero, seguido de un salto de línea.

```C
// Escribe en el archivo utilizando fprintf
fprintf(archivo, "¡Hola mundo! %d\n", 7);
```

Finalmente, debes cerrar el archivo con la función `fclose()`.

```C
// Cerrar el archivo
fclose(archivo);
```

El resultado de este código será que se creará un archivo llamado "datos.txt" en la misma carpeta que contiene el programa. Si ya existe un archivo con ese nombre, el contenido anterior será reemplazado.

## Profundizando:

La escritura de archivos de texto es una técnica ampliamente utilizada en la programación, ya que permite guardar y leer información de manera sencilla y estructurada. Otros lenguajes de programación también tienen funciones similares para escribir en archivos de texto, como `print()` en Python o `write()` en Java.

Para escribir en archivos binarios (que no son legibles para los humanos), se pueden utilizar otras funciones como `fwrite()` en C. Además, existen bibliotecas que facilitan la escritura de archivos más complejos, como la biblioteca `csv` para manipular archivos CSV (valores separados por comas).

## Ver también:

- [Funciones de manejo de archivos en C](https://www.programiz.com/c-programming/c-file-input-output)
- [Biblioteca estándar de C](https://es.wikipedia.org/wiki/Biblioteca_est%C3%A1ndar_de_C)