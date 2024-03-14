---
date: 2024-01-20 17:32:46.982365-07:00
description: "Comparar dos fechas significa verificar si son iguales, cu\xE1l es anterior\
  \ o posterior. Los programadores lo hacen para rastrear eventos, validar per\xED\
  odos\u2026"
lastmod: '2024-03-13T22:44:59.387429-06:00'
model: gpt-4-1106-preview
summary: "Comparar dos fechas significa verificar si son iguales, cu\xE1l es anterior\
  \ o posterior. Los programadores lo hacen para rastrear eventos, validar per\xED\
  odos\u2026"
title: "Comparaci\xF3n de dos fechas"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Comparar dos fechas significa verificar si son iguales, cuál es anterior o posterior. Los programadores lo hacen para rastrear eventos, validar períodos de tiempo, o controlar vigencias.

## Cómo:
```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // Crear fecha con formato AAAA, MM, DD
    std::tm date1 = {};
    date1.tm_year = 2021 - 1900; // Año desde 1900
    date1.tm_mon = 7; // Mes comenzando desde 0
    date1.tm_mday = 19; // Día del mes

    std::tm date2 = {};
    date2.tm_year = 2022 - 1900;
    date2.tm_mon = 7;
    date2.tm_mday = 19;

    // Convertir a tiempo tipo time_t
    std::time_t time1 = std::mktime(&date1);
    std::time_t time2 = std::mktime(&date2);

    // Compara las fechas
    if (time1 < time2) {
        std::cout << "La fecha1 es anterior a la fecha2." << std::endl;
    } else if (time1 > time2) {
        std::cout << "La fecha1 es posterior a la fecha2." << std::endl;
    } else {
        std::cout << "Las fechas son iguales." << std::endl;
    }

    return 0;
}
```
Salida de muestra: `La fecha1 es anterior a la fecha2.`

## Análisis Profundo
Antes, la gestión de fechas en C++ no era sencilla. Se usaba `struct tm` que venía con sus complicaciones. Con C++11, llegó `<chrono>`, una modernización que simplifica la manipulación del tiempo. Pero, aún utilizamos `mktime` y `time_t` para comparar fechas, porque es un método directo y mucho código antiguo todavía lo usa.

Alternativas modernas incluyen `std::chrono::system_clock` para representar puntos en el tiempo. Uno puede obtener la duración entre dos fechas con `<chrono>` y calcular diferencias más naturalmente.

Detalles de implementación:
- Si se comparan fechas y horas, también se deben establecer `tm_hour`, `tm_min`, `tm_sec`.
- Cuidado con la zona horaria. `std::mktime` usa la zona horaria local por defecto.

## Ver Además
- Documentación de `<chrono>` de C++: https://en.cppreference.com/w/cpp/header/chrono
- Tutorial de fechas y horas en C++: https://www.learncpp.com/cpp-tutorial/date-and-time/
- Comparar `time_t` en C: https://www.cplusplus.com/reference/ctime/time_t/
