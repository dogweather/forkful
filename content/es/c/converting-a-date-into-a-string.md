---
title:                "Convirtiendo una fecha en una cadena de texto"
date:                  2024-01-20T17:36:06.001960-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convirtiendo una fecha en una cadena de texto"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Convertir una fecha a cadena de texto significa transformar un objeto de fecha y hora en una serie de caracteres legibles. Esto se hace para mostrar fechas en interfaces, almacenarlas de forma compatible con sistemas de bases de datos, o para realizar operaciones de registro y seguimiento.

## Cómo hacerlo:
La biblioteca estándar de C brinda funciones para manejar fechas y cadenas de caracteres. Un ejemplo es `strftime`, que formatea una fecha a texto.

```C
#include <stdio.h>
#include <time.h>

int main() {
    char fecha_str[100];
    time_t ahora = time(NULL);
    struct tm *tiempo = localtime(&ahora);

    // Formato YYYY-MM-DD
    strftime(fecha_str, sizeof(fecha_str), "%Y-%m-%d", tiempo);
    printf("Fecha actual: %s\n", fecha_str);

    // Formato DD/MM/YYYY
    strftime(fecha_str, sizeof(fecha_str), "%d/%m/%Y", tiempo);
    printf("Fecha en otro formato: %s\n", fecha_str);

    return 0;
}
```

Salida:
```
Fecha actual: 2023-04-12
Fecha en otro formato: 12/04/2023
```

## Inmersión profunda:
La función `strftime` se remonta a las primeras versiones de la biblioteca estándar de C, basándose en las capacidades de manejo de tiempo de sistemas Unix. Es ampliamente adoptada por su simplicidad y eficacia.

Alternativas incluyen el uso de `sprintf`, `snprintf`, implementaciones propias para plataformas que no soportan `strftime` o incluso funciones de bibliotecas de terceros.

Detalles de implementación:
- `%Y`, `%m`, `%d` son especificadores de formato para año, mes y día.
- `time_t` y `struct tm` son tipos de datos de C que representan el tiempo en segundos desde la llamada Epoch (1970-01-01 00:00:00 UTC) y una estructura de fecha y hora desglosada, respectivamente.
- `time` obtiene el tiempo actual y `localtime` lo convierte a la hora local.

## Ver también:
- Documentación de `strftime`: https://en.cppreference.com/w/c/chrono/strftime
- Tutoriales de manejo de fechas y horas en C: https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm
- Conceptos Unix de tiempo: https://www.gnu.org/software/libc/manual/html_node/Time-Types.html
