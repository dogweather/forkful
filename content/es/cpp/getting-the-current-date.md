---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
En programación, obtener la fecha actual implica capturar el día, mes y año en tiempo real. Los programadores lo hacen para registrar eventos, crear marcas de tiempo y para funciones que requieren fecha y hora, como calcular la diferencia entre fechas.

## Cómo hacerlo:
Aquí te mostramos cómo obtener la fecha actual en C++ utilizando la librería `<chrono>`.

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    auto now = std::chrono::system_clock::now();
    std::time_t currentTime = std::chrono::system_clock::to_time_t(now);

    std::cout << "La fecha actual es: " << std::ctime(&currentTime);
    return 0;
}
```

Este código producirá una salida similar a esta:

`La fecha actual es: Mon Feb 14 22:55:20 2022`

## Conociendo más:
Históricamente, C++ no tenía una sólida biblioteca de fecha/hora hasta C++11, que introdujo el módulo `<chrono>`. Antes de eso, los programadores tenían que apoyarse en bibliotecas de C y estructuras de datos para manejar las fechas y horas.

Existe otra biblioteca común llamada `<ctime>` que incluye funciones para obtener la fecha y la hora actual. Sin embargo, `<chrono>` es más flexible y segura.

La precisión de `<chrono>` también es una mejora significativa respecto a las alternativas. Funciona con un reloj de alta resolución y puede proporcionar la hora hasta en nanosegundos.

## Ver Tambien:
1. Documentación oficial de `<chrono>`: http://www.cplusplus.com/reference/chrono/
2. Tutorial de Time Library: https://www.geeksforgeeks.org/c-time-library/
3. C++ Date and Time: https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm