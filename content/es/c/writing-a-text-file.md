---
title:                "Escritura de un archivo de texto"
html_title:           "C: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir un archivo de texto en C?

Escribir un archivo de texto en C puede ser útil para almacenar datos o configuraciones que se pueden acceder y modificar fácilmente desde un programa. También puede ser una forma eficiente de guardar información en un formato legible para los usuarios.

## Cómo hacerlo

Para escribir un archivo de texto en C, primero se debe abrir el archivo utilizando la función "fopen" y especificando el nombre del archivo y el modo de apertura. Luego, se pueden utilizar las funciones "fprintf" o "fputs" para escribir datos en el archivo, seguidas de la función "fclose" para cerrar correctamente el archivo.

```C
#include <stdio.h>

int main() {
  FILE *archivo;
  char texto[] = "¡Hola mundo!";
  // Abrir archivo en modo de escritura
  archivo = fopen("archivo.txt", "w");
  // Escribir en el archivo
  fprintf(archivo, "%s", texto);
  // Cerrar el archivo
  fclose(archivo);
  return 0;
}
```

**Salida:**

```
[archivo.txt]
¡Hola mundo!
```

## Profundizando

Los archivos de texto se pueden abrir en diferentes modos, como lectura, escritura o anexar. También es importante tener en cuenta la ruta del archivo, que puede ser una ruta absoluta o una ruta relativa al directorio actual. Además, al escribir en un archivo, es importante asegurarse de que los datos estén formateados correctamente, ya que un solo error puede corromper todo el archivo.

## Ver también

- [Funciones de archivo en C](https://www.programiz.com/c-programming/c-file-input-output)
- [Tutorial completo de C](https://www.learn-c.org/) 
- [Documentación oficial de C](https://devdocs.io/c/)