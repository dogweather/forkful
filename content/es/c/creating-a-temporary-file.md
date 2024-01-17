---
title:                "Creando un archivo temporal"
html_title:           "C: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué hacerlo?

Los programadores a menudo se encuentran en la necesidad de crear archivos temporales en sus programas. Estos son archivos que se usan para almacenar información temporalmente mientras el programa se está ejecutando. Esto puede ser útil para guardar datos que no son necesarios después de la ejecución del programa, o para mantener un registro de ciertos cálculos o procesos.

## Cómo hacerlo:

Para crear un archivo temporal en C, se puede utilizar la función ```tmpfile()```. Esta función crea un archivo temporal y devuelve un puntero al mismo. A continuación, se puede escribir y leer en el archivo temporal como si fuera un archivo normal. Una vez que el programa se cierra, el archivo temporal se elimina automáticamente.

```
#include <stdio.h>

int main() {
   FILE *tempfile = tmpfile();
   
   if (tempfile != NULL) {
      fputs("Datos temporales", tempfile);
      rewind(tempfile);
      char buffer[20];
      fgets(buffer, 20, tempfile);
      puts(buffer);
   }
   
   return 0;
}
```

La salida de este programa sería:

```
Datos temporales
```

## Profundizando:

Crear archivos temporales es una práctica común en la programación, especialmente en sistemas operativos basados en Unix. Sin embargo, también hay alternativas como utilizar variables temporales en la memoria o utilizar la función ```tempnam()``` para crear un archivo con un nombre específico.

Cabe mencionar que los archivos temporales también pueden ser utilizados para tareas de seguridad, como guardar contraseñas encriptadas temporalmente.

En cuanto a la implementación, la función ```tmpfile()``` utiliza el sistema de archivos temporal del sistema operativo para crear el archivo. Esto significa que la ubicación y el nombre del archivo pueden variar dependiendo del sistema en el que se esté ejecutando el programa.

## Ver también:

- [Documentación de la función tmpfile() de C](https://www.gnu.org/software/libc/manual/html_node/Creating-a-Temporary-File.html)
- [Otras funciones relacionadas con archivos temporales en C](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)