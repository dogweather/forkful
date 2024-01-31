---
title:                "Escritura de un archivo de texto"
date:                  2024-01-19
html_title:           "Bash: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qué es y por qué?

Escribir un archivo de texto en C es guardar datos en un formato legible por humanos. Los programadores lo hacen para almacenar configuraciones, registros o compartir información entre programas.

## Cómo hacerlo:

```c
#include <stdio.h>

int main() {
    FILE *file;
    
    // Abriendo el archivo para escritura. "w" para modo escritura.
    file = fopen("ejemplo.txt", "w");
    
    if(file == NULL) {
        printf("Error al abrir el archivo.\n");
        return 1;
    }
    
    // Escribiendo texto en el archivo.
    fprintf(file, "Hola, Mundo!\n");
    
    // Cerrando el archivo.
    fclose(file);
    
    return 0;
}
```

Salida en `ejemplo.txt`:
```
Hola, Mundo!
```

## Análisis Profundo:

Históricamente, la escritura de archivos en C se ha manejado con las funciones estándar del I/O provistas en `stdio.h`. Alternativas modernas incluyen I/O de bajo nivel con `sys/stat.h` o I/O de bibliotecas externas como POSIX. Detalles de implementación incluyen gestión correcta de errores y cerrar siempre los archivos para evitar fugas de memoria.

## Ver también:

- Documentación oficial de GNU C `stdio`: https://www.gnu.org/software/libc/manual/html_node/Output-Streams.html
- Tutorial de manejo de archivos en C: https://www.tutorialspoint.com/cprogramming/c_file_io.htm
- Referencia de I/O POSIX: https://pubs.opengroup.org/onlinepubs/9699919799/functions/write.html
