---
title:                "C++: Calculando una fecha en el futuro o en el pasado"
simple_title:         "Calculando una fecha en el futuro o en el pasado"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo puedes saber una fecha específica en el futuro o en el pasado? ¡Programando en C++ puedes hacerlo fácilmente! En este post te explicaremos cómo puedes calcular una fecha con tan solo unas líneas de código.

## Cómo hacerlo

Primero, debes asegurarte de incluir la biblioteca de fecha y hora de C++ en tu programa:

```C++
#include <ctime>
```

A continuación, necesitas declarar las variables para el día, el mes y el año:

```C++
int dia, mes, año;
```

Luego, puedes utilizar la función `time_t` para obtener la fecha y hora actual:

```C++
time_t fecha_actual = time(0);
```

Después de eso, puedes utilizar la función `localtime` para obtener la fecha y hora local actual:

```C++
struct tm* fecha_local = localtime(&fecha_actual);
```

Ahora, puedes modificar la fecha y hora utilizando la función `mktime`:

```C++
struct tm fecha_deseada = *fecha_local; // En este ejemplo, estamos usando la fecha actual como fecha deseada
fecha_deseada.tm_mday += 5; // Sumamos 5 días a la fecha actual
fecha_deseada.tm_mon += 1; // Sumamos 1 mes a la fecha actual
fecha_deseada.tm_year += 1; // Sumamos 1 año a la fecha actual
```

Finalmente, puedes imprimir la fecha deseada utilizando la función `strftime`:

```C++
char fecha[80];
strftime(fecha, 80, "%d/%m/%y", &fecha_deseada);
std::cout << "La fecha deseada es: " << fecha << std::endl;
```

Y ¡listo! Ahora sabes cómo calcular una fecha en el futuro o en el pasado utilizando C++.

## Profundizando

Si quieres profundizar más en el tema, puedes investigar sobre la forma en que C++ maneja las fechas y horas utilizando la estructura `tm` y las funciones `time`, `localtime`, `mktime` y `strftime`.

También puedes aprender sobre otras funciones y técnicas que puedes utilizar para trabajar con fechas y horas, como la función `difftime`, que calcula la diferencia en segundos entre dos fechas, o la biblioteca boost::date_time, que proporciona una gran cantidad de funcionalidades para trabajar con fechas y horas.

## Ver también
- [Documentación de fechas y horas en C++](https://en.cppreference.com/w/cpp/chrono)
- [C++ date_time library](https://www.boost.org/doc/libs/1_70_0/doc/html/date_time.html)