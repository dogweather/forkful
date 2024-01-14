---
title:                "C: Calculando una fecha en el futuro o el pasado"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo calcular una fecha en el futuro o en el pasado en un programa escrito en C? Esto puede ser útil en varios casos, como en aplicaciones de calendario o gestión de eventos. Aprender a hacer esto en C te permitirá agregar una funcionalidad más avanzada a tus programas.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado, primero necesitamos la fecha actual. En C, podemos obtener la fecha actual utilizando la función `time()`, que devuelve el número de segundos transcurridos desde una fecha de referencia. Luego, utilizando algunos cálculos matemáticos simples, podemos agregar o restar un número determinado de días, meses o años a la fecha actual para obtener la fecha deseada. Veamos un ejemplo de código que calcula la fecha dentro de 100 días a partir de la fecha actual:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Obtener fecha actual
    time_t now = time(NULL);
    // Definir número de segundos en 100 días
    time_t hundred_days = 100 * 24 * 60 * 60;
    // Calcular fecha dentro de 100 días
    time_t future_date = now + hundred_days;
    // Convertir fecha a estructura `tm`
    struct tm *future_date_struct = localtime(&future_date);
    // Imprimir resultado en formato DD-MM-AAAA
    printf("La fecha dentro de 100 días será: %d-%d-%d", future_date_struct->tm_mday, future_date_struct->tm_mon + 1, future_date_struct->tm_year + 1900);

    return 0;
}
```

La salida del programa será:

```
La fecha dentro de 100 días será: 6-1-2022
```

Recordemos que los cálculos de fechas pueden variar según la zona horaria en la que nos encontremos, por lo que es importante tener esto en cuenta al realizar cálculos precisos de fechas.

## Profundizando

Para calcular fechas aún más precisas, también podemos utilizar la función `mktime()` que nos permite crear una estructura `tm` con una fecha y hora específicas. Además, existen varias funciones para manipular fechas como `strftime()`, que permite dar formato a una fecha en una cadena de caracteres, y `difftime()`, que calcula la diferencia de tiempo entre dos fechas.

También es importante tener en cuenta que existen bibliotecas externas en C que facilitan el cálculo de fechas, como `libical` o `libdatetime`.

## Ver también

- [Documentación de la función `time()` en C](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)
- [Explicación detallada de cómo calcular fechas en C](https://www.programiz.com/c-programming/examples/current-date-time)
- [Biblioteca de C para cálculos de fechas](https://libical.github.io/libical/)