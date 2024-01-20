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

## ¿Qué y por qué?

Comparar dos fechas significa determinar cuál de las dos fechas es anterior, posterior o si son iguales. Los programadores realizan esta tarea para ordenar eventos, calcular períodos de tiempo, establecer recordatorios y más.

## ¿Cómo hacerlo?

Para comparar dos fechas en C++, podemos utilizar la biblioteca `<chrono>` de la librería estándar. Aquí tienes un ejemplo:

```C++
#include <iostream>
#include <chrono>

int main() {
  std::chrono::system_clock::time_point hoy = std::chrono::system_clock::now();
  std::chrono::system_clock::time_point manana = hoy + std::chrono::hours(24);

  if (hoy < manana) {
    std::cout << "Hoy es antes que mañana." << std::endl;
  } else {
    std::cout << "Hoy no es antes que mañana." << std::endl;
  }
  return 0;
}
```

Este programa va a imprimir "Hoy es antes que mañana." porque `hoy` es 24 horas menos que `manana`.

## Análisis detallado

La biblioteca `<chrono>` es parte de la C++ Standard Library desde C++11. Ofrece tipos que representan puntos de tiempo, duraciones y relojes. Antes de su introducción, los programadores debian recurrir a funciones C menos seguras y menos expresivas como `time()` y `difftime()`.

Alternativas a `<chrono>` incluyen bibliotecas de terceros como Boost.DateTime o los tipos nativos de tiempo y fecha de sistemas de bases de datos.

En cuanto a la implementación, `<chrono>` es muy eficiente y segura al comparar fechas. Los objetos `time_point` son representaciones de un punto en el tiempo, y las operaciones de comparación no implican ninguna conversión o cálculo complejo.

## Ver también

Para más información, visita estos recursos:

1. [Documentación oficial de <chrono>](https://en.cppreference.com/w/cpp/chrono)
2. [La biblioteca Boost.DateTime](https://www.boost.org/doc/libs/1_68_0/doc/html/date_time.html)
3. [Tutorial de C++11 <chrono>](https://www.modernescpp.com/index.php/the-three-clocks)