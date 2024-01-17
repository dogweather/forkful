---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "C: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Calcular una fecha en el futuro o pasado es una técnica utilizada por los programadores para determinar una fecha específica basada en una fecha dada. Esto puede ser útil en aplicaciones que requieren proyecciones de fechas, como programación de reservas o recordatorios de eventos.

## Cómo:
Para calcular una fecha en el futuro o pasado en C, podemos utilizar la función `mktime` de la librería `time.h`. Esta función toma una estructura `tm` que contiene los componentes de una fecha (día, mes, año, hora, etc.) y devuelve un valor de tipo `time_t` que representa los segundos transcurridos desde la época Unix. Podemos entonces sumar o restar este valor según sea necesario para obtener la fecha deseada.

Ejemplo de código para calcular la fecha 7 días en el futuro a partir de la fecha actual:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Obtener la fecha y hora actual
    time_t today;
    time(&today);

    // Convertir a estructura tm
    struct tm* current = localtime(&today);

    // Sumar 7 días, mktime se encarga de ajustar la fecha
    current->tm_mday += 7;

    // Convertir de vuelta a time_t
    time_t future = mktime(current);

    // Imprimir la fecha en formato dd/mm/aaaa
    printf("Fecha en 7 días: %02d/%02d/%4d\n", current->tm_mday, 
           current->tm_mon + 1, current->tm_year + 1900);

    return 0;
}
```

Salida:

```
Fecha en 7 días: 18/03/2021
```

## Profundizando:
La función `mktime` es una implementación de la norma Posix y se basa en el tiempo de Unix, que cuenta los segundos transcurridos desde el 1 de enero de 1970 a las 00:00 am UTC. Este formato de fecha y hora es ampliamente utilizado en sistemas operativos y lenguajes de programación.

Otra alternativa para calcular una fecha en el futuro o pasado es utilizar la librería `libical`, que es una implementación de la norma iCalendar y permite trabajar con fechas y horas de forma más avanzada.

Es importante tener en cuenta que al trabajar con fechas y horas, es fundamental considerar zonas horarias y ajustar las fechas según el horario de verano de cada región. Es por eso que es recomendable utilizar librerías especializadas en el manejo de fechas y horas en lugar de implementar nuestra propia lógica.

## Ver también:
- [Documentación de la función `mktime` en la librería `time.h` (en inglés)](https://en.cppreference.com/w/c/chrono/mktime)
- [Documentación de la librería `libical` (en inglés)](http://libical.github.io/libical/)