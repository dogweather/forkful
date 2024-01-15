---
title:                "Comparando dos fechas"
html_title:           "C++: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

Comparamos fechas por varias razones, incluyendo la organización y clasificación de datos, planificación de eventos y automatización de tareas en programas. Saber cómo comparar fechas correctamente puede ser útil en muchas situaciones diferentes en la programación.

## Cómo hacerlo

La comparación de fechas en C++ se realiza mediante el uso de la estructura "tm" de la biblioteca estándar "time.h". Primero, declaramos dos variables de tipo "tm" para almacenar las fechas que queremos comparar, utilizando el formato "dd/mm/aaaa". Luego, usamos funciones como "mktime" y "difftime" para convertir las fechas a segundos y obtener la diferencia entre ellas. Finalmente, podemos comparar la diferencia obtenida con un valor predefinido para determinar cuál es la fecha más reciente.

``` C++
#include <iostream>
#include <time.h>

using namespace std;

int main()
{
    // Declarar variables de tipo tm
    struct tm fecha1, fecha2;

    // Asignar valores a las fechas
    fecha1.tm_mday = 10; // día 10
    fecha1.tm_mon = 5; // mes 5 (junio)
    fecha1.tm_year = 2020; // año 2020

    fecha2.tm_mday = 5; // día 5
    fecha2.tm_mon = 5; // mes 5 (junio)
    fecha2.tm_year = 2020; // año 2020

    // Convertir fechas a segundos
    time_t tiempo1 = mktime(&fecha1);
    time_t tiempo2 = mktime(&fecha2);

    // Calcular la diferencia entre fechas
    double diferencia = difftime(tiempo1, tiempo2);

    // Comparar la diferencia con un valor predefinido (0 en este caso)
    if (diferencia > 0) {
        cout << "La fecha 1 es más reciente que la fecha 2";
    } else if (diferencia < 0) {
        cout << "La fecha 2 es más reciente que la fecha 1";
    } else {
        cout << "Ambas fechas son iguales";
    }

    return 0;
}
```

Output:
```
La fecha 1 es más reciente que la fecha 2
```

## Profundizando

En la comparación de fechas, es importante tener en cuenta que el formato de fecha puede variar dependiendo de la región o el sistema operativo en el que estamos trabajando. Por ejemplo, en algunas regiones, el formato puede ser "mm/dd/aaaa" en lugar de "dd/mm/aaaa". Por lo tanto, es importante verificar siempre el formato de fecha antes de realizar cualquier operación de comparación.

También es importante tener en cuenta que la conversión de fechas a segundos puede resultar en valores inexactos debido a la naturaleza de los cálculos internos utilizados. Por lo tanto, es recomendable utilizar funciones de biblioteca como "difftime" para obtener una diferencia precisa entre fechas.

## Ver también

- [Comparación de strings en C++](https://www.programiz.com/cpp-programming/string-comparison)
- [Estructuras de datos en C++](https://www.cplusplus.com/doc/tutorial/structures/)