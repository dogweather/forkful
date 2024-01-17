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

## ¡Qué es y por qué es importante!
Comparar dos fechas es una tarea común para los programadores. Es simplemente comparar dos valores de fecha y determinar si son iguales, mayores o menores entre sí. Los programadores hacen esto para asegurarse de que la información se ordene correctamente y para tomar decisiones basadas en la fecha, como mostrar eventos futuros o caducados.

## ¡Cómo hacerlo!
Aquí hay un ejemplo de cómo comparar dos fechas en C++:

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // Definimos dos fechas
    time_t fecha1, fecha2;
    fecha1 = time(nullptr); // Fecha actual
    fecha2 = 1600702800; // 22 de septiembre de 2020

    // Comparamos las fechas
    if (fecha1 < fecha2) {
        cout << "Fecha1 es menor que Fecha2" << endl;
    } else if (fecha1 > fecha2) {
        cout << "Fecha1 es mayor que Fecha2" << endl;
    } else {
        cout << "Fecha1 es igual a Fecha2" << endl;
    }

    return 0;
}
```

La salida para este ejemplo sería: "Fecha1 es mayor que Fecha2".

## Profundizando
La comparación de fechas tiene su origen en la necesidad de ordenar y organizar eventos y actividades en un calendario, tanto en la vida diaria como en la informática. Además del método de comparación utilizado en el ejemplo anterior (operadores de comparación), también existen otras formas de comparar fechas, como utilizando funciones de biblioteca o librerías externas.

## Vea también:
- [Documentación de C++ en fechas y horarios](https://en.cppreference.com/w/cpp/chrono) 
- [Cómo trabajar con fechas en C++](https://www.studytonight.com/cpp/date-and-time-functions.php)