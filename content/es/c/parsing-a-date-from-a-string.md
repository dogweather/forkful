---
title:                "Análisis de una fecha a partir de una cadena"
date:                  2024-01-20T15:34:59.507424-07:00
html_title:           "Arduino: Análisis de una fecha a partir de una cadena"
simple_title:         "Análisis de una fecha a partir de una cadena"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Parsear una fecha de una cadena significa convertir el texto que representa una fecha a una estructura de datos que el programa pueda entender y manipular. Lo hacemos porque, en el mundo real, las fechas a menudo vienen en forma de strings y necesitamos trabajar con ellas de manera más precisa y funcional.

## Cómo hacerlo:
Veamos cómo se convierte una cadena que representa una fecha a una estructura `tm` de C.

```C
#include <stdio.h>
#include <time.h>

int main() {
    const char *fecha_str = "2023-04-01";
    struct tm fecha_tm;

    // intentamos parsear la fecha
    if (strptime(fecha_str, "%Y-%m-%d", &fecha_tm) == NULL) {
        printf("Error al parsear la fecha.\n");
        return 1;
    }

    // imprimimos la estructura para demostrar el éxito del parsing
    printf("Año: %d, Mes: %d, Día: %d\n", fecha_tm.tm_year + 1900, fecha_tm.tm_mon + 1, fecha_tm.tm_mday);

    return 0;
}
```

Salida de muestra:

```
Año: 2023, Mes: 4, Día: 1
```

## Análisis Detallado
El parsing de fechas es casi tan antiguo como la programación misma. Se necesitaba una forma estandarizada de interpretar fechas para distintos usos, como bases de datos o interfaces de usuario.

En C, `strptime` es una función estandarizada pero no parte de C89 o C99 estándar, y sí incluida en POSIX. Podría no estar disponible en todas las plataformas. Alternativas incluyen las funciones como `scanf` o librerías de terceros.

La clave para entender `strptime` está en los formatos de fecha/hora. `%Y`, `%m`, y `%d` representan año, mes y día respectivamente. Lo importante es asegurarse que el formato especificado coincida con la cadena que se está parseando.

## Véase También
- Información sobre `strptime` y sus formatos en la página de manual: https://man7.org/linux/man-pages/man3/strptime.3.html
- Alternativas de parsing de fechas en C: https://www.cplusplus.com/reference/ctime/get_time/
- Documentación POSIX: https://pubs.opengroup.org/onlinepubs/9699919799/
