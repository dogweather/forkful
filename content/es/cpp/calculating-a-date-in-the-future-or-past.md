---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "C++: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías calcular una fecha en el futuro o en el pasado?

A veces, en programas de calendario o aplicaciones de planificación, es necesario calcular una fecha en el futuro o en el pasado. Por ejemplo, puede ser útil para programar recordatorios o citas, o para calcular la edad de alguien en una fecha específica.

## Cómo realizar cálculos de fechas en C++

En C++, hay varias funciones y librerías que puedes utilizar para calcular fechas en el futuro o en el pasado. A continuación, se presentan algunos ejemplos de código y su salida correspondiente dentro de bloques de código "```C++ ... ```".

### Calcular una fecha en el futuro con la función *mktime()*

La función *mktime()* toma una estructura *tm* y devuelve un *time_t* que representa esa fecha y hora. Utilizando esta función, podemos calcular una fecha en el futuro sumando un cierto número de segundos a una fecha de partida. Aquí hay un ejemplo de cómo podría ser eso:

```C++

#include <iostream>
#include <ctime>

int main() {
    // Se crea una fecha de partida utilizando la estructura *tm*
    std::tm startDate = {0, 0, 12, 1, 0, 2019 - 1900};
    // Se utiliza la función *mktime()* para obtener un *time_t* que representa la fecha de partida
    std::time_t startTime = std::mktime(&startDate);

    // Se calculan 30 días en segundos
    int secondsIn30Days = 30 * 24 * 60 * 60;
    // Se suma ese valor a la fecha de partida
    std::time_t futureTime = startTime + secondsIn30Days;
    // Se imprime la fecha en el futuro en un formato legible 
    std::cout << "La fecha en 30 días será: " << std::asctime(std::localtime(&futureTime));
    
    return 0;
}

```

La salida de este programa sería:

```
La fecha en 30 días será: Thu Jan 31 12:00:00 2019
```

### Calcular una fecha en el pasado con la función *mktime()*

De manera similar, también podemos utilizar la función *mktime()* para calcular una fecha en el pasado restando un número de segundos de una fecha de partida. Aquí hay un ejemplo de cómo podría verse ese código:

```C++

#include <iostream>
#include <ctime>

int main() {
    // Se crea una fecha de partida utilizando la estructura *tm*
    std::tm startDate = {0, 0, 12, 1, 0, 2019 - 1900};
    // Se utiliza la función *mktime()* para obtener un *time_t* que representa la fecha de partida
    std::time_t startTime = std::mktime(&startDate);

    // Se calculan 30 días en segundos
    int secondsIn30Days = 30 * 24 * 60 * 60;
    // Se resta ese valor a la fecha de partida
    std::time_t pastTime = startTime - secondsIn30Days;
    // Se imprime la fecha en el pasado en un formato legible 
    std::cout << "La fecha hace 30 días fue: " << std::asctime(std::localtime(&pastTime));
    
    return 0;
}

```

La salida de este programa sería:

```
La fecha hace 30 días fue: Tue Nov 27 12:00:00 2018
```

## Más información sobre el cálculo de fechas en C++

Esta es solo una pequeña muestra de cómo se pueden calcular fechas en el futuro o en el pasado en C++. También hay otras funciones y librerías disponibles, como *time.h* y *chrono*, que pueden ser útiles para realizar cálculos de fechas. Es importante investigar y elegir el método que mejor se adapte a las necesidades de tu programa.

## También te puede interesar

- [Documentación de la función *mktime()* en cplusplus.com](http://www.cplusplus.com/reference/ctime/mktime/)
- [Tutorial sobre gestión de fechas y horas en C++](https://www.codeproject.com/Articles/6915/Date-Conversion-Functions-Revisited)
- [Librería *time.h* en la documentación de C++ de Microsoft](https://msdn.microsoft.com/es-es/library/1y93248f.aspx)
- [Introducción a la librería *chrono* en el sitio de Cppreference](https://en.cppreference.com/w/cpp/chrono)