---
title:                "Escribiendo en el error estándar"
date:                  2024-01-19
simple_title:         "Escribiendo en el error estándar"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Escribir en el error estándar permite mostrar mensajes de error separados de la salida normal del programa. Los programadores lo hacen para depurar y notificar problemas sin interferir con la salida que usuarios o programas esperan.

## How to:
```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Error: algo salió mal.\n");
    return 0;
}
```
Salida de muestra en la consola:
```
Error: algo salió mal.
```

## Deep Dive
En los primeros días de Unix, la idea de tener canales separados para errores y salida regular fue establecida. `stderr`, un stream predefinido en C, es no-buffered, asegurando que los mensajes de error se muestren inmediatamente. Alternativas incluyen escribir a un archivo de log o usar `perror()` para errores estándares del sistema. A nivel de implementación, `stderr` está asociado con el descriptor de archivo 2 en sistemas POSIX.

## See Also
- Documentación de GNU sobre `stdio.h`: [https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
- Una discusión sobre `stderr` en Stack Overflow: [https://stackoverflow.com/questions/39002052/how-i-can-print-to-stderr-in-c](https://stackoverflow.com/questions/39002052/how-i-can-print-to-stderr-in-c)
- Tutorial de manejo de errores en C: [https://www.tutorialspoint.com/cprogramming/c_error_handling.htm](https://www.tutorialspoint.com/cprogramming/c_error_handling.htm)
