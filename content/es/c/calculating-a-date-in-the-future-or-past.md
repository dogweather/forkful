---
title:                "C: Calculando una fecha en el futuro o pasado."
simple_title:         "Calculando una fecha en el futuro o pasado."
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Por qué

Calcular fechas en el futuro o en el pasado es una herramienta esencial para muchas aplicaciones, como calendarios, gestión de tareas y automatización de procesos. Además, es un concepto fundamental en la programación que permite comprender cómo funcionan las fechas en los diferentes lenguajes de programación.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado en C, primero debemos tener en cuenta algunas cosas importantes:

- La fecha actual se puede obtener fácilmente con la función `time()` de la librería `time.h`.
- Debemos tener en cuenta el formato de la fecha que queremos obtener, ya sea en formato día/mes/año o mes/día/año.
- La estructura `struct tm` nos permite trabajar con fechas de manera más sencilla, ya que almacena los diferentes componentes de una fecha (día, mes, año, etc.).

Una vez establecidas estas consideraciones, podemos utilizar las funciones `mktime()` y `localtime()` para obtener una estructura `struct tm` de una fecha en particular. Luego, podemos utilizar operaciones matemáticas para sumar o restar días, meses o años a la estructura `struct tm`, y finalmente convertirlo de nuevo a tiempo utilizando la función `mktime()`.

A continuación, se muestra un ejemplo de cómo calcular una fecha en el futuro y en el pasado utilizando las consideraciones mencionadas anteriormente:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Obtenemos la fecha actual
    time_t now = time(NULL);

    // Convertimos la fecha actual a una estructura `struct tm`
    struct tm *tm_now = localtime(&now);

    // Sumamos 10 días a la fecha actual
    tm_now->tm_mday += 10;

    // Convertimos de nuevo la estructura `struct tm` a tiempo
    time_t new_date = mktime(tm_now);

    // Imprimimos la nueva fecha en formato día/mes/año
    printf("Fecha en 10 días: %02d/%02d/%d\n", tm_now->tm_mday, tm_now->tm_mon + 1, tm_now->tm_year + 1900);

    // Restamos 2 meses a la fecha actual
    tm_now->tm_mon -= 2;

    // Convertimos de nuevo la estructura `struct tm` a tiempo
    new_date = mktime(tm_now);

    // Imprimimos la nueva fecha en formato día/mes/año
    printf("Fecha hace 2 meses: %02d/%02d/%d\n", tm_now->tm_mday, tm_now->tm_mon + 1, tm_now->tm_year + 1900);

    return 0;
}
```

El resultado de este código sería:

```
Fecha en 10 días: 22/08/2021
Fecha hace 2 meses: 22/05/2021
```

## Profundizando

Calcular fechas en el futuro o en el pasado puede ser un poco más complejo cuando se tienen en cuenta otros factores, como los años bisiestos, cambio de horario de verano/invierno, entre otros. En esos casos, es importante comprender cómo funcionan estas situaciones en el lenguaje de programación y aplicar ajustes necesarios en el código.

Una recomendación importante es utilizar librerías externas que ya se encarguen de estos cálculos complejos, como la librería `date.h`, que facilita la manipulación de fechas y horas en diferentes formatos.

# Ver también

- [Documentación oficial de la librería `time.h`](https://www.gnu.org/software/libc/manual/html_node/Working-with-Time.html)
- [Documentación de la librería `date.h`](https://github.com/HowardHinnant/date)