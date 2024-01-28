---
title:                "Manejo de errores"
date:                  2024-01-26T00:36:42.323320-07:00
model:                 gpt-4-1106-preview
simple_title:         "Manejo de errores"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/handling-errors.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Manejar errores en C es anticiparse a lo inesperado. Evita que los programas se descontrolen cuando encuentran problemas. Los programadores lo hacen para manejar los errores con gracia y mantener su código fiable.

## Cómo hacerlo:

Veamos cómo hacerlo en C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("archivo_inexistente.txt", "r");
    if (fp == NULL) {
        perror("Error al abrir el archivo");
        return EXIT_FAILURE;
    }
    // Hacer algo con el archivo
    fclose(fp);
    return EXIT_SUCCESS;
}
```

Salida de muestra cuando el archivo no existe:
```
Error al abrir el archivo: No existe el fichero o el directorio
```

## Análisis en profundidad

En los primeros días de C, el manejo de errores era básico: principalmente códigos de retorno y comprobaciones manuales. Luego llegó `errno`, una variable global que se actualiza cuando las funciones fallan. Sin embargo, de por sí, no es segura entre hilos, así que se introdujeron las funciones `strerror` y `perror` para mejorar la notificación de errores.

¿Alternativas? El C moderno no se limita a `errno`. Están setjmp y longjmp para saltos no locales cuando ocurre un desastre. Algunas personas prefieren definir sus propios códigos de error, mientras que otras optan por estructuras similares a las excepciones en C++.

Los detalles de implementación pueden ser complejos. Por ejemplo, `errno` es seguro entre hilos en sistemas compatibles con POSIX debido a la magia del Almacenamiento Local de Hilos (TLS). En sistemas embebidos, donde los recursos son preciados, se podría preferir el código personalizado de manejo de errores en lugar de los enfoques estándar que podrían sobrecargar el software.

## Vea también

- Un análisis detallado de `errno`: https://en.cppreference.com/w/c/error/errno
- Para la seguridad entre hilos, vea hilos POSIX y errno: http://man7.org/linux/man-pages/man3/pthread_self.3.html
- Una introducción a setjmp y longjmp: https://www.cplusplus.com/reference/csetjmp/
- Para el manejo de excepciones en C++, consulte: https://isocpp.org/wiki/faq/exceptions
