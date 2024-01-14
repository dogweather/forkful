---
title:    "C: Creación de un archivo temporal"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Por qué crear un archivo temporal en programación?

Crear archivos temporales es una tarea común en la programación. Estos archivos se utilizan para almacenar temporalmente datos importantes durante la ejecución de un programa. También son útiles para realizar pruebas o pruebas de concepto sin afectar a los archivos permanentes. En resumen, crear un archivo temporal es una forma eficiente de manejar datos transitorios en un programa.

## Cómo crear un archivo temporal en C

Para crear un archivo temporal en C, utilizamos la función `tmpfile()` de la biblioteca `<stdio.h>`. Esta función reserva un archivo temporal en el sistema y devuelve un puntero a él. Luego podemos escribir y leer datos en este archivo como lo haríamos con cualquier otro archivo.

```C
#include <stdio.h>
int main() {
   FILE *fp;
   fp = tmpfile();
   fprintf(fp, "Este es un archivo temporal creado en C.");
   rewind(fp);
   char buffer[100];
   fgets(buffer, 100, fp);
   printf("Datos del archivo temporal: %s\n", buffer);
   fclose(fp);
   return 0;
}
```

El resultado de este código será:

```
Datos del archivo temporal: Este es un archivo temporal creado en C.
```

## Profundizando en la creación de archivos temporales

En el código anterior, utilizamos la función `tmpfile()` para crear un archivo temporal. Sin embargo, esta función solo funciona en sistemas Unix y no se puede utilizar en Windows. En su lugar, podemos usar la función `fopen()` y proporcionar al final del nombre de archivo la palabra clave `"tmp"` para indicar que es un archivo temporal.

Otra cosa importante a tener en cuenta al crear archivos temporales es que se eliminan automáticamente al finalizar el programa. Sin embargo, si queremos eliminarlos manualmente, podemos usar la función `remove()` proporcionándole el nombre del archivo temporal.

## Ver también
- Tutorial para principiantes en programación en C: [https://www.tutorialspoint.com/cprogramming/index.htm](https://www.tutorialspoint.com/cprogramming/index.htm)
- Documentación de la función `tmpfile()`: [https://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html](https://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html)
- Diferencias entre Windows y Unix en la creación de archivos temporales: [https://stackoverflow.com/questions/3737210/temporary-files-in-unix-vs-windows](https://stackoverflow.com/questions/3737210/temporary-files-in-unix-vs-windows)