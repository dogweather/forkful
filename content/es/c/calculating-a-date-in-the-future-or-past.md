---
title:                "Calcular una fecha en el futuro o pasado"
date:                  2024-01-20T17:30:53.635193-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcular una fecha en el futuro o pasado"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Calcular una fecha en el futuro o pasado es simplemente añadir o sustraer días a una fecha dada. Los programadores lo hacen para gestionar eventos, programar recordatorios o calcular plazos.

## Cómo hacerlo:
```C
#include <stdio.h>
#include <time.h>

void calcularFechaFutura(int dias) {
    time_t ahora = time(NULL);
    struct tm nuevaFecha = *localtime(&ahora);

    nuevaFecha.tm_mday += dias;  
    mktime(&nuevaFecha);  // Normaliza la fecha

    char buffer[30];
    strftime(buffer, 30, "%Y-%m-%d", &nuevaFecha);
    printf("La fecha %d días en el futuro es: %s\n", dias, buffer);
}

int main() {
    int dias = 10;
    calcularFechaFutura(dias);
    return 0;
}
```
**Salida de muestra:**
```
La fecha 10 días en el futuro es: 2023-04-21
```

## Análisis Profundo
Históricamente, calcular fechas ha sido un desafío debido a las numerosas anomalías en calendarios y husos horarios. Alternativas para realizar esta operación en C incluyen librerías como `<time.h>` y funciones como `gmtime()` y `mktime()`, las cuales facilitan la manipulación de fechas de manera estándar y portable. Al implementar el cálculo de fechas, se ajustan automáticamente los valores de meses y años, manejan años bisiestos y consideran el cambio de hora si es necesario.

## Ver También
- Manual de GNU C Library: https://www.gnu.org/software/libc/manual/html_node/Time-and-Date.html
- Página de `strftime`: https://en.cppreference.com/w/c/chrono/strftime
- Ejemplos de `mktime`: https://www.cplusplus.com/reference/ctime/mktime/
