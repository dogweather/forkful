---
title:                "Creando un archivo temporal"
date:                  2024-01-20T17:39:47.560296-07:00
model:                 gpt-4-1106-preview
simple_title:         "Creando un archivo temporal"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Crear un archivo temporal significa hacer un archivo que se usa mientras corre tu programa pero normalmente se borra cuando el programa termina. Los programadores lo hacen para guardar datos que no necesitan permanecer después de que el programa se ha ejecutado, como información de sesión o para evitar colisiones de nombres en archivos.

## Cómo Hacerlo:
El estándar de C no tiene una función incorporada específica para crear archivos temporales, pero puedes usar `tmpfile()` de la biblioteca estándar para crear un archivo temporal que se borra automáticamente, o `mkstemp()` si necesitas más control.

### tmpfile()
```C
#include <stdio.h>

int main() {
    FILE *tmp = tmpfile();
    if (tmp == NULL) {
        perror("No se pudo crear el archivo temporal.");
        return 1;
    }

    fputs("Esto se escribe en un archivo temporal.", tmp);
    
    // El archivo se borra cuando se cierra.
    fclose(tmp);
    
    return 0;
}
```

### mkstemp()
```C
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
    char template[] = "/tmp/miarchivoXXXXXX";
    int fd = mkstemp(template);
    if (fd == -1) {
        perror("Error al crear el archivo temporal con mkstemp.");
        return 1;
    }

    // Usa fd para escribir en el archivo.
    write(fd, "Esto es un ejemplo con mkstemp", 30);
    
    // Cierra y borra manualmente el archivo.
    close(fd);
    unlink(template);
    
    return 0;
}
```

## Buceando Profundo:
Crear archivos temporales es una técnica que se remonta a los inicios del desarrollo de software, usado para manejar datos que no necesitan sobrevivir más allá de la instancia actual del programa en ejecución.

### Alternativas
Alternativas a `tmpfile()` y `mkstemp()` incluyen `mktemp()` y `tempnam()`, aunque estas son menos seguras por riesgo de colisiones de nombres y deberían evitarse.

### Implementación
`tmpfile()` crea un archivo temporal en el directorio predeterminado para archivos temporales. `mkstemp()` requiere un patrón con `XXXXXX` que será reemplazado con caracteres que garantizan un nombre de archivo único; este enfoque da más control.

## Ver También:
- [Documentación de `tmpfile`](https://en.cppreference.com/w/c/io/tmpfile)
- [Documentación de `mkstemp`](https://man7.org/linux/man-pages/man3/mkstemp.3.html)
- Guías sobre practicas seguras para crear y manejar archivos temporales, como [CERT's guide on secure temp file usage](https://wiki.sei.cmu.edu/confluence/display/c/FIO21-C.+Do+not+create+temporary+files+in+shared+directories)
