---
title:                "Análisis de una fecha a partir de una cadena"
date:                  2024-01-20T15:35:11.056063-07:00
html_title:           "Arduino: Análisis de una fecha a partir de una cadena"
simple_title:         "Análisis de una fecha a partir de una cadena"

category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Parsear una fecha desde una cadena de texto significa convertir el formato de texto a una estructura de fecha que el programa pueda entender y manipular. Los programadores hacemos esto para poder comparar fechas, cambiar formatos o calcular intervalos de tiempo de manera eficiente.

## Cómo hacerlo:

En C++, desde C++11 en adelante, se utiliza la biblioteca `<chrono>` junto con `<sstream>` y `<iomanip>` para parsear fechas. A continuación, un ejemplo:

```C++
#include <iostream>
#include <sstream>
#include <iomanip>
#include <chrono>

int main() {
    std::string fecha_texto = "28-03-2023";
    std::istringstream stream(fecha_texto);
    std::tm tm = {};
    stream >> std::get_time(&tm, "%d-%m-%Y");

    if(stream.fail()) {
        std::cout << "Formato de fecha inválido." << std::endl;
    } else {
        std::chrono::system_clock::time_point fecha_parseada = std::chrono::system_clock::from_time_t(std::mktime(&tm));
        std::cout << "Fecha parseada correctamente." << std::endl;
    }

    return 0;
}
```

Salida esperada:

```
Fecha parseada correctamente.
```

## Profundizando:

Antes de C++11, los programadores dependían de las funciones `strptime` y `strftime` de las bibliotecas de C. Aunque funcionales, no eran tan seguras ni fáciles de manejar como las actuales de `<chrono>`. 

Alternativas a `<chrono>` podrían ser bibliotecas de terceros como Boost.Date_Time, sin embargo, desde que `<chrono>` se ha incluido en el estándar, su uso es más recomendado por ser parte de la biblioteca estándar.

Con respecto a los detalles de implementación, usando `<chrono>` y `<iomanip>`, gestionamos la interpretación de fechas de manera más segura y con mejores prácticas, facilitando la internacionalización y las operaciones con zonas horarias.

## Ver También:

- Documentación de C++ sobre `<chrono>`: https://en.cppreference.com/w/cpp/header/chrono
- Más sobre `<sstream>` y `<iomanip>`: https://en.cppreference.com/w/cpp/header/sstream https://en.cppreference.com/w/cpp/header/iomanip
- Tutorial sobre el manejo de fechas y horas en C++: https://www.learncpp.com/cpp-tutorial/8-16-stdchrono-library-time-duration-and-clocks/ 
- Boost.Date_Time, para más funcionalidades de fechas y horas: https://www.boost.org/doc/libs/release/libs/date_time/
