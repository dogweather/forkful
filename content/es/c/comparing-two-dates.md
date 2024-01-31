---
title:                "Comparación de dos fechas"
date:                  2024-01-20T17:32:22.113004-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparación de dos fechas"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Comparar dos fechas es el proceso de determinar la relación temporal entre ellas: cuál es anterior o si son iguales. Los programadores lo hacen para gestionar eventos, calcular períodos, o validar cronologías en sus programas.

## Cómo hacerlo:

Para comparar dos fechas en C, generalmente recurrimos a la biblioteca `time.h`. Te muestro un ejemplo sencillo y su resultado esperado.

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm fecha1 = {0};
    struct tm fecha2 = {0};

    // Configurar fecha 1: 1 de Enero de 2023
    fecha1.tm_year = 123; // Año desde 1900
    fecha1.tm_mon = 0;    // Mes, desde Enero = 0
    fecha1.tm_mday = 1;   // Día del mes

    // Configurar fecha 2: 15 de Febrero de 2023
    fecha2.tm_year = 123; // Año desde 1900
    fecha2.tm_mon = 1;    // Mes, desde Enero = 0
    fecha2.tm_mday = 15;  // Día del mes

    // Convertir struct tm a type time_t
    time_t tiempo1 = mktime(&fecha1);
    time_t tiempo2 = mktime(&fecha2);

    // Comparar
    if (tiempo1 < tiempo2) {
        printf("La primera fecha es anterior a la segunda.\n");
    } else if (tiempo1 > tiempo2) {
        printf("La segunda fecha es anterior a la primera.\n");
    } else {
        printf("Ambas fechas son iguales.\n");
    }

    return 0;
}
```

Resultado esperado:
```
La primera fecha es anterior a la segunda.
```

## Detalles Profundos:

Antes, comparar fechas era más complicado, manejando los componentes manualmente. `time.h` ofreció una solución estandarizada que simplificaba el proceso. Existen alternativas, como la función `difftime()` que calcula la diferencia en segundos entre dos `time_t`, pero para una simple comparación, `<` y `>` son suficientes. Internamente, `mktime()` convierte un `struct tm` a `time_t`, que es un entero representando segundos desde la "epoch" (Jan 1, 1970). Esto hace fácil la comparación.

## Ver También:

- Manuales en línea de `time.h`: https://en.cppreference.com/w/c/chrono
- Documentación de la biblioteca estándar en C: https://www.cplusplus.com/reference/ctime/
- Guía de conceptos básicos sobre fechas y horas en C: https://www.tutorialspoint.com/c_standard_library/time_h.htm
