---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "C++: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué hacerlo?

Calcular una fecha en el futuro o en el pasado es un proceso en el que los programadores utilizan un lenguaje de programación para determinar una fecha determinada en relación a otra fecha. Esto se puede hacer por diversas razones, como planear eventos futuros o realizar cálculos de tiempo.

## Cómo hacerlo:

A continuación se presentan ejemplos de código en C++ para calcular una fecha en el futuro y en el pasado:

```
// Calcula una fecha 5 días en el futuro
#include <iostream>
#include <ctime>
using namespace std;

int main() {
  // Obtiene la fecha actual
  time_t now = time(0);
  
  // Convierte la fecha actual a struct tm para manipulación de tiempo
  tm* today = localtime(&now);
  
  // Agrega 5 días a la fecha actual
  today->tm_mday += 5;
  
  // Convierte la nueva fecha a formato legible
  time_t future = mktime(today);
  cout << "La fecha 5 días en el futuro es: " << ctime(&future);
  
  return 0;
}

```
La salida de este ejemplo sería: "La fecha 5 días en el futuro es: (fecha en formato legible)".

Ahora veamos cómo calcular una fecha en el pasado:

```
// Calcula una fecha 2 meses en el pasado
#include <iostream>
#include <ctime>
using namespace std;

int main() {
  // Obtiene la fecha actual
  time_t now = time(0);
  
  // Convierte la fecha actual a struct tm para manipulación de tiempo
  tm* today = localtime(&now);
  
  // Resta 2 meses a la fecha actual
  today->tm_mon -= 2;
  
  // Convierte la nueva fecha a formato legible
  time_t past = mktime(today);
  cout << "La fecha 2 meses en el pasado es: " << ctime(&past);
  
  return 0;
}

```
La salida de este ejemplo sería: "La fecha 2 meses en el pasado es: (fecha en formato legible)".

## Profundizando:

- Contexto histórico: Calcular fechas en el futuro o en el pasado se ha vuelto más relevante con el avance de la tecnología y la necesidad de tener una visión precisa del tiempo en el mundo digital. Esto se ha vuelto especialmente importante en aplicaciones empresariales y de gestión del tiempo.
- Alternativas: Además de C++, existen otros lenguajes de programación como Java, Python y JavaScript que también pueden ser utilizados para calcular fechas en el futuro o en el pasado.
- Detalles de implementación: En C++, se pueden utilizar funciones como time() y mktime() para obtener y manipular fechas en forma de estructuras de tiempo (struct tm).

## Ver también:

- [Funciones de tiempo en C++](https://es.cppreference.com/w/cpp/chrono/c/tm)
- [Tutorial de fechas y tiempos en C++](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
- [Ejemplos adicionales de cálculos de fechas en C++](https://www.w3schools.com/cpp/cpp_date_calculations.asp)