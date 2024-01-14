---
title:                "C: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por Qué

Comparar dos fechas puede ser una tarea común en la programación. Puede ayudarnos a realizar acciones basadas en la fecha actual, como mostrar una alerta cuando la fecha límite de un proyecto se acerca. En esta publicación, exploraremos cómo comparar dos fechas en el lenguaje de programación C.

## Cómo

En C, podemos trabajar con fechas utilizando la estructura `tm` y la función `mktime()`. La estructura `tm` contiene la información de fecha y hora, y la función `mktime()` nos permite convertir esta información en un valor de tipo `time_t`, que representa el número de segundos desde el 1 de enero de 1970. Utilizaremos esta función para convertir nuestras dos fechas en valores `time_t` antes de compararlas.

A continuación, mostraremos un ejemplo de código que compara dos fechas y muestra un mensaje si la primera fecha es anterior a la segunda:

```C
#include <stdio.h>
#include <time.h>

int main(){
    // Primera fecha
    struct tm date1 = { .tm_year = 2020, .tm_mon = 5, .tm_mday = 25 };

    // Segunda fecha
    struct tm date2 = { .tm_year = 2021, .tm_mon = 3, .tm_mday = 10 };

    // Convertir fechas a valores time_t
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // Comparar fechas
    if(time1 < time2){
        printf("La primera fecha es anterior a la segunda.");
    }

    return 0;
}
```

El código anterior producirá la siguiente salida:

```
La primera fecha es anterior a la segunda.
```

Existen otras funciones en C que también nos permiten comparar fechas, como `difftime()` y `localtime()`, que pueden ser útiles dependiendo de la situación. Recomendamos explorar estas funciones más a fondo para descubrir cómo pueden encajar en tus proyectos.

## Deep Dive

Es importante tener en cuenta que la precisión de la comparación de fechas en C depende de la precisión del reloj de la máquina en la que se está ejecutando. Si la precisión del reloj es de solo un segundo, por ejemplo, la comparación de fechas solo puede ser precisa hasta ese nivel. También es importante manejar adecuadamente la zona horaria en la que se encuentran las fechas que estás comparando, ya que esto también puede afectar los resultados.

Además, debemos tener en cuenta que en algunos casos, como cuando se trabaja con años bisiestos, la comparación de fechas puede ser más complicada y requerir el uso de funciones adicionales para garantizar la precisión.

## Ver También
- [Documentación oficial de C en fecha y hora](https://en.cppreference.com/w/c/chrono)
- [Más información sobre la estructura tm y la función mktime()](https://www.tutorialspoint.com/c_standard_library/c_function_mktime.htm)
- [Ejemplo de aplicación práctica de comparación de fechas en C](https://www.geeksforgeeks.org/compare-two-dates-c/)