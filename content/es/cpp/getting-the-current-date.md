---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:13:22.433323-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?
Obtener la fecha actual en un programa es conocer el día presente según el calendario. Los programadores lo hacen para registrar eventos, comparar fechas o simplemente mostrar información relevante al usuario.

## Cómo hacerlo:
En C++, puedes usar `<chrono>` y `<iostream>` para obtener y mostrar la fecha actual:

```C++
#include <iostream>
#include <chrono>
#include <iomanip>
#include <ctime>

int main() {
    auto now = std::chrono::system_clock::now(); // Obtiene el tiempo actual
    std::time_t now_time = std::chrono::system_clock::to_time_t(now); 

    // Usa `std::localtime` para convertir a estructura `tm`
    std::tm *ptm = std::localtime(&now_time);
    
    // Imprime la fecha con formato yyyy-mm-dd
    std::cout << std::put_time(ptm, "%Y-%m-%d") << std::endl;

    return 0;
}
```

Salida de muestra:
```
2023-04-01
```

## Análisis Profundo:
Históricamente, C++ manejaba fechas y tiempos usando las funciones de C como `time()` y `localtime()`. Desde C++11, el manejo del tiempo se ha modernizado con la biblioteca `<chrono>`, permitiendo alta precisión y operaciones con duraciones.

Alternativas para manejar fechas incluyen bibliotecas de terceros como Boost.DateTime, que ofrece aún más funciones.

La implementación usando `<chrono>` es segura y preferida en C++ moderno. Al trabajar con `std::chrono::system_clock`, generalmente obtenemos la hora en UTC, pero `std::localtime` la ajusta a la zona horaria local del sistema.

## Ver También:
- Documentación de `<chrono>`: https://en.cppreference.com/w/cpp/header/chrono
- Time in C++ (Artículo sobre tiempo en C++): https://www.cplusplus.com/reference/ctime/
- Boost.DateTime: https://www.boost.org/doc/libs/release/libs/date_time/