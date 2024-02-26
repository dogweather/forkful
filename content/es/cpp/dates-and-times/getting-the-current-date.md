---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:06.106143-07:00
description: "Obtener la fecha actual en C++ es una tarea fundamental para programas\
  \ que necesitan procesar o mostrar fechas basadas en el reloj del sistema. Es\u2026"
lastmod: '2024-02-25T18:49:55.855419-07:00'
model: gpt-4-0125-preview
summary: "Obtener la fecha actual en C++ es una tarea fundamental para programas que\
  \ necesitan procesar o mostrar fechas basadas en el reloj del sistema. Es\u2026"
title: Obteniendo la fecha actual
---

{{< edit_this_page >}}

## Qué y Por Qué
Obtener la fecha actual en C++ es una tarea fundamental para programas que necesitan procesar o mostrar fechas basadas en el reloj del sistema. Es esencial para el registro (logging), estampado de tiempo (time-stamping), la programación de tareas y cualquier funcionalidad que dependa de fechas y hora.

## Cómo hacerlo:
C++ ofrece varias maneras de obtener la fecha actual, incluyendo la biblioteca estándar de C++ y bibliotecas de terceros como Boost. Los siguientes ejemplos demuestran cómo lograr esta tarea.

### Usando `<chrono>` (C++20 y posteriores)
C++20 introdujo más funcionalidades en la biblioteca `<chrono>`, haciendo sencillo obtener la fecha actual:
```cpp
#include <iostream>
#include <chrono>
#include <format> // Para std::format (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // Captura el tiempo actual
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // Convertir a time_t

    // Formatear el tiempo a un formato legible
    std::cout << "Fecha Actual: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**Salida de Ejemplo:**
```plaintext
Fecha Actual: 2023-03-15
```

### Usando `<ctime>`
Para programadores trabajando con versiones más antiguas de C++ o aquellos que prefieren la biblioteca tradicional de C:
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // Obtener el tiempo actual
    std::tm* now = std::localtime(&t);
    std::cout << "Fecha Actual: " 
              << (now->tm_year + 1900) << '-' 
              << (now->tm_mon + 1) << '-'
              <<  now->tm_mday
              << std::endl;

    return 0;
}
```
**Salida de Ejemplo:**
```plaintext
Fecha Actual: 2023-03-15
```

### Usando Boost Date_Time
Para proyectos que utilizan las bibliotecas Boost, la biblioteca Boost Date_Time ofrece un método alternativo para obtener la fecha actual:
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // Obtener el día actual usando el calendario gregoriano de Boost
    boost::gregorian::date today = boost::gregorian::day_clock::local_day();
    std::cout << "Fecha Actual: " << today << std::endl;

    return 0;
}
```
**Salida de Ejemplo:**
```plaintext
Fecha Actual: 2023-Mar-15
```
Estos ejemplos proporcionan una base sólida para trabajar con fechas en C++, crucial para una amplia gama de aplicaciones.
