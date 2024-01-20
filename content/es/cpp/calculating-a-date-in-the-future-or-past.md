---
title:                "Cálculo de una fecha en el futuro o pasado"
html_title:           "C++: Cálculo de una fecha en el futuro o pasado"
simple_title:         "Cálculo de una fecha en el futuro o pasado"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Calcular una Fecha Futura o Pasada en C++
## ¿Qué y Por qué?
El cálculo de una fecha en el futuro o el pasado es una función clave en la programación de computadoras que permite prever o retroceder en el tiempo. Se hace uso de esto en muchas aplicaciones, como la planificación de eventos, los recordatorios o la programación de tareas.

## ¿Cómo Hacerlo?
Aquí mostramos cómo calcular una fecha futura o pasada en C++.

```C++
#include <iostream>
#include <ctime>

int main() {
    struct std::tm t = {0};
    t.tm_year = 2021-1900;
    t.tm_mon = 11;
    t.tm_mday = 27;

    std::mktime(&t);

    // sumar o restar días
    t.tm_mday += 5;  
    std::mktime(&t);

    std::cout << 1900 + t.tm_year << "-" << 1 + t.tm_mon << 
    "-" << t.tm_mday << "\n";
    return 0;
}
```
Este programa dará como resultado una fecha cinco días a partir del 27 de noviembre de 2021.

## Análisis Detallado
El cálculo de una fecha en el futuro o en el pasado es una antigua necesidad en la programación. Antes del estandarizado Time Library en C++, los programadores utilizaban métodos propios para calcular fechas, lo que llevaba a una considerable duplicación de esfuerzos. Ahora, `ctime` brinda una forma estandarizada y sencilla de hacerlo.

Una alternativa a este método, podría ser el uso de la biblioteca Chrono de C++ para hacer cálculos de tiempo más precisos y avanzados.

Los detalles de implementación incluyen la utilización de la estructura `tm` y la función `mktime`. Aquí, el año está representado desde 1900, por lo que debes restar 1900 de la entrada del año. El mes está basado en cero, por lo que deberías incrementarlo en 1 para tu salida.

## Vea También
Para más información, consulta los enlaces a documentación relevante a continuación:
1. Documentación oficial de C++ Library – time.h: https://www.cplusplus.com/reference/ctime/
2. Tutorial de manipulación de Fechas y Tiempo en C++: https://www.learnCpp.com/cpp-tutorial/89-class-scope-and-accessing-class-data/
3. Biblioteca Chrono en C++: https://en.cppreference.com/w/cpp/chrono