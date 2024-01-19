---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?

*Parsear* una fecha de un *string* se trata de convertir una representación textual de una fecha a una representación de fecha que un programa puede entender y manipular. Los programadores hacen esto para procesar datos de entrada, trabajar con APIs, o para descomponer y reorganizar información de fechas.

# Cómo se hace:

En C++11 y versiones posteriores, la biblioteca de manipulación de fechas y horas, `<chrono>`, puede usarse para parsear fechas a partir de strings. Veamos un ejemplo:

```C++
#include <iostream>
#include <sstream>
#include <iomanip>
#include <chrono>

int main() {
    // string que representa la fecha
    std::string fecha_string = "2022-01-20";

    // Creamos un stringstream con la fecha
    std::istringstream ss(fecha_string);

    // Buffer para almacenar la fecha parseada
    std::chrono::system_clock::time_point fecha;

    // Creamos el objeto para el formateo de la fecha
    std::chrono::time_parse("{%Y-%m-%dT%H:%M:%S}", fecha, ss);

    // Imprimimos la fecha
    std::time_t tt = std::chrono::system_clock::to_time_t(fecha);
    std::cout << std::put_time(std::localtime(&tt), "%Y-%m-%d %H:%M:%S") << '\n';

    return 0;
}
```
Salida esperada:
```
2022-01-20 00:00:00
```

# Detalles

El parsing de fechas en C++ ha cambiado con el tiempo. Antes de C++11, tenías que usar `strptime` o `sscanf`, funciones de C que pueden ser propensas a errores. La implementación actual de `<chrono>` y `date::parse` es robusta y fiable.

Hay varias alternativas para parsear fechas, algunas de las cuales son bibliotecas de terceros como Boost.Date_Time y HowardHinnant's date library. Escoge la que mejor se adapte a tus necesidades.

Los detalles de implementación del parsing de fechas de un string pueden ser bastante complejos, ya que hay que tener en cuenta zonas horarias, formatos de fecha locales y todo tipo de variaciones.

## Ver También

- Documentación de `<chrono>`: http://www.cplusplus.com/reference/chrono/
- La biblioteca de fechas de Boost: https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html
- Las fechas se vuelven modernas en C++: https://www.modernescpp.com/index.php/the-dates-of-modern-c