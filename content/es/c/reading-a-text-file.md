---
title:                "Lectura de un archivo de texto"
date:                  2024-01-20T17:53:47.541034-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lectura de un archivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Leer un archivo de texto en C es obtener su contenido para usarlo en tu programa. Programadores lo hacen para manejar datos, configuraciones o cualquier información almacenada en archivos.

## Cómo hacerlo:
Para leer un archivo, primero debemos abrirlo, luego leer su contenido y finalmente cerrarlo. A continuación, un ejemplo práctico:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *archivo;
    char linea[100]; // Asumimos que cada línea tiene menos de 100 caracteres

    // Abrimos el archivo para lectura
    archivo = fopen("ejemplo.txt", "r");
    if (archivo == NULL) {
        perror("Error al abrir archivo");
        return 1;
    }

    // Leemos el archivo línea por línea
    while (fgets(linea, sizeof(linea), archivo) != NULL) {
        printf("%s", linea);
    }

    // Cerramos el archivo
    fclose(archivo);

    return 0;
}
```

Output:
```
Primera línea del archivo
Segunda línea del archivo
...
```

## Análisis Detallado
Leer archivos es fundamental desde los primeros días de programación. Históricamente, el acceso a archivos es lento comparado con la memoria, así que es importante leer de manera eficiente.

Alternativas a `fgets` incluyen `fscanf` para leer formatos específicos, o `read` en sistemas Unix para un control más bajo nivel. C11, la versión actual de C, mantiene las mismas funciones básicas para leer archivos, pero siempre revisa las actualizaciones del estándar.

Detalles de implementación:
- Usamos `FILE` para representar un archivo en C.
- `fopen` abre el archivo. `"r"` significa "solo lectura".
- `fgets` lee hasta que encuentra un newline o alcanza el límite del buffer.
- `fclose` cierra el archivo y libera recursos. Es importante no olvidarse de cerrar cada archivo que abres.

## Ver Además
- [Tutorial de C File I/O en Programiz](https://www.programiz.com/c-programming/c-file-input-output)
- [Referencia de C Standard Library en cppreference.com](https://en.cppreference.com/w/c/io)
- [Ejemplos y Documentación de GNU C Library](https://www.gnu.org/software/libc/manual/html_node/I_002fO-Overview.html)