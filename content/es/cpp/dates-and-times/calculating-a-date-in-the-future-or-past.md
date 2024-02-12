---
title:                "Calcular una fecha en el futuro o pasado"
aliases:
- /es/cpp/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:09.800571-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcular una fecha en el futuro o pasado"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Calcular una fecha futura o pasada es simplemente sumar o restar días a una fecha dada. Los programadores lo hacen para manejar eventos, vencimientos o cualquier lógica que dependa del tiempo.

## Cómo hacerlo:
Para calcular fechas en C++, podemos usar la biblioteca `<chrono>` junto con `<ctime>` y funciones como `std::mktime` y `std::localtime`. Aquí tienes un ejemplo básico:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // Obteniendo la fecha actual
    std::time_t now = std::time(nullptr);
    
    // Convirtiéndola a fecha local
    std::tm* now_tm = std::localtime(&now);
    
    // Añadir 10 días a la fecha actual
    now_tm->tm_mday += 10; 
    
    // Normalizar y convertir a time_t
    std::time_t future_time = std::mktime(now_tm);
    
    // Convertir a string para legibilidad
    std::cout << "La fecha en 10 días será: " << std::asctime(now_tm) << std::endl;

    return 0;
}
```

Salida de muestra:
```
La fecha en 10 días será: Wed Mar 23 11:10:52 2023
```

## Análisis Profundo:
Historicamente, C++ manejaba fechas y tiempo mediante `<ctime>`, que es parte de la biblioteca de C. Sin embargo, esto podía resultar complejo y propenso a errores. Con C++11 y versiones posteriores, se introdujo `<chrono>`, modernizando y simplificando la manipulación del tiempo.

Hay alternativas a `<chrono>` y `<ctime>`, como la popular biblioteca de terceros Boost.Date_Time. Aunque es potente, usar `<chrono>` es suficiente para la mayoría de necesidades y viene incluido en el estándar de C++.

Detalles de implementación importantes:
- La estructura `std::tm` no maneja meses y días de la semana basándose en 1, sino en 0, por lo que `tm_mon` va de 0 a 11 y `tm_wday` va de 0 a 6.
- Es crucial normalizar la fecha con `std::mktime` después de modificar la estructura `tm` para ajustar los valores de hora, día, mes y año.

## Ver También:
- Documentación oficial de `<chrono>` en CppReference: https://en.cppreference.com/w/cpp/header/chrono
- Guía de Boost.Date_Time: https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html
- Tutorial de `<ctime>`: https://www.cplusplus.com/reference/ctime/
