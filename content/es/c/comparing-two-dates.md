---
title:                "C: Comparando dos fechas"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

Comparar dos fechas es una tarea común en la programación. Puede ser útil para determinar cuál de las dos fechas es más reciente o calcular la diferencia de tiempo entre ellas. Además, es una habilidad básica que todo programador debería tener.

## Cómo hacerlo

Para comparar dos fechas en C, podemos utilizar la función `difftime`. Esta función toma dos argumentos, ambos en formato de tiempo `time_t`, y devuelve la diferencia en segundos entre ellos.

```C
// Ejemplo: Comparar dos fechas
#include <stdio.h>
#include <time.h>

int main()
{
    time_t date1, date2;
    double diff;

    // Definir las fechas a comparar
    date1 = time(NULL);
    date2 = mktime(&date1);

    // Calcular la diferencia en segundos
    diff = difftime(date1, date2);

    // Imprimir la diferencia
    printf("La diferencia entre las dos fechas es de %.2f segundos.", diff);

    return 0;
}
```

El código anterior imprimirá la diferencia en segundos entre la fecha actual y la fecha en la que se ejecutó el programa.

## Profundizando

En C, las fechas se almacenan en el formato `time_t`, que representa el número de segundos desde el 1 de enero de 1970 a las 00:00:00 UTC. Por lo tanto, la función `difftime` simplemente resta el número `time_t` de la segunda fecha del número de la primera fecha.

Sin embargo, hay ciertos casos en los que la comparación de fechas puede ser más complicada. Por ejemplo, si se trata de fechas de años diferentes, con diferentes cantidades de días, o si se desea comparar solo el mes y el día sin considerar el año. En estos casos, es necesario utilizar otras funciones de la biblioteca `time.h`, como `localtime` y `mktime`, junto con la manipulación de estructuras de tiempo.

Si deseas profundizar más en la comparación de fechas en C, te recomendamos leer más acerca de funciones como `localtime`, `strftime`, `mktime` y `difftime`. Estas funciones pueden ser muy útiles en diferentes situaciones donde necesitas comparar fechas de manera más específica.

## Ver también

- [Funciones de fecha y hora en C](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)
- [Ejemplos de comparación de fechas en C](https://www.geeksforgeeks.org/compare-two-dates-c/)
- [Manipulación de estructuras de tiempo en C](https://www.programiz.com/c-programming/c-date-time)