---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Crear un archivo temporal es hacer un archivo que se borra después de cerrarlo o de reiniciar el PC. Los programadores lo hacen para almacenar datos volátiles y ahorrar memoria.

## ¿Cómo hacerlo?

Vamos a crear y escribir en un archivo temporal.

```C
#include <stdio.h>

int main() {
    char temp[] = "/tmp/tempfileXXXXXX";
    int fd = mkstemp(temp);

    if (fd == -1) {
        printf("Error al crear el archivo temporal\n");
        return 1;
    }

    dprintf(fd, "Hola Mundo");

    close(fd);
    return 0;
}
```

Este programa creará un archivo llamado "tempfileXXXXXX" en /tmp y escribirá "Hola Mundo" en él.

## Análisis en Profundidad

**Contexto histórico** - El uso de archivos temporales se remonta a los días de las tarjetas perforadas, donde eran una forma de gestionar la memoria limitada.

**Alternativas** - Puede usar RAM para almacenar temporalmente los datos pero este enfoque consume mucha memoria. También puedes usar bases de datos, pero un archivo temporal es más simple y rápido.

**Detalles de implementación** - La función mkstemp() genera un nombre de archivo único y lo abre para escritura. Al usar mkstemp(), asegúrate de cerrar el descriptor de archivo cuando hayas terminado.

## Ver También

* El manual de C: [mkstemp] (http://man7.org/linux/man-pages/man3/mkstemp.3.html)
* Guía para trabajar con archivos en C: [File IO in C] (https://www.geeksforgeeks.org/file-operations-in-c/)
* Elegir entre usar archivos temporales o RAM: [Tempfiles VS RAM] (https://www.linuxjournal.com/article/6672)