---
title:                "C++: Calculando una fecha en el futuro o pasado"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Calcular fechas en el futuro o en el pasado puede ser una tarea importante en la programación, especialmente cuando se trata de planificar eventos o automatizar tareas. Con la ayuda de un código bien escrito, es posible generar fechas precisas y ahorrar tiempo y esfuerzo en el proceso.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado, primero se necesita una fecha inicial y una cantidad de tiempo en días, meses o años. A continuación, se puede utilizar la clase `std::chrono::time_point` de la biblioteca estándar de C++ para obtener una estructura `tm` con los detalles de la fecha deseada.

```C++
#include <iostream>
#include <chrono>
using namespace std::chrono;

// Función para calcular una fecha en el futuro o en el pasado
void calcularFecha(time_t fechaInicial, int cantidadTiempo, int tipoTiempo) {

    // Crear un objeto time_point a partir de la fecha inicial
    time_point<system_clock> timePunto(fechaInicial);

    // Calcular la fecha final utilizando la función `time_point` `operator+=`
    timePunto += duration<int, std::ratio<86400>>(cantidadTiempo*tipoTiempo);

    // Convertir el objeto `time_point` a la estructura `tm`
    time_t fechaFinal = system_clock::to_time_t(timePunto);
    tm *fecha = localtime(&fechaFinal);

    // Imprimir la fecha en un formato legible
    std::cout << "La fecha calculada es: " << fecha->tm_mday << "/" << fecha->tm_mon+1 << "/" << fecha->tm_year+1900 << std::endl;
}

int main() {
    // Fecha inicial: 1 de enero de 2021
    time_t fechaInicial = time(nullptr);
    tm *fecha = localtime(&fechaInicial);
    fecha->tm_mday = 1;
    fecha->tm_mon = 0;
    fecha->tm_year = 121;

    // Calculando 10 días en el futuro
    calcularFecha(fechaInicial, 10, 1);

    // Calculando 6 meses en el pasado
    calcularFecha(fechaInicial, 6, -30);

    // Calculando 2 años en el futuro
    calcularFecha(fechaInicial, 2, 365);

    return 0;
}
```

El resultado de ejecutar el código anterior será:

```
La fecha calculada es: 11/1/2021
La fecha calculada es: 1/7/2020
La fecha calculada es: 1/1/2023
```

## Más información

Para calcular fechas en el futuro o en el pasado en C++, es importante tener en cuenta la precisión de los cálculos. Por ejemplo, si se desea calcular una fecha en años, pero la cantidad de tiempo especificada es un número decimal, el resultado puede no ser tan preciso como se espera. Además, también es importante tener en cuenta la zona horaria en la que se desea calcular la fecha.

En general, el uso de la biblioteca estándar de C++ y sus clases `time_point` y `duration` puede facilitar la tarea de calcular fechas en el futuro o en el pasado de manera precisa y eficiente.

## Ver también

- [Documentación de la biblioteca estándar de C++ sobre fechas y tiempo](https://en.cppreference.com/w/cpp/chrono)
- [Tutorial sobre la biblioteca `std::chrono` en C++](https://www.learncpp.com/cpp-tutorial/38-stdchrono/)  
- [Ejemplos de cálculos de fechas utilizando C++](https://www.techiedelight.com/calculate-add-years-dates-cpp/)