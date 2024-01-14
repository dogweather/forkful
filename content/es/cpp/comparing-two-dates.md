---
title:    "C++: Comparando dos fechas"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por Qué
En la programación, a menudo necesitamos comparar fechas para realizar diferentes acciones. Ya sea para validar una tarjeta de crédito, calcular la edad o simplemente para ordenar eventos cronológicamente, trabajar con fechas es una tarea común en el desarrollo de software. En este artículo, te mostraremos cómo comparar dos fechas en C++ y profundizar en el tema para que puedas implementarlo en tus proyectos.

## Cómo Hacerlo
Para comparar dos fechas en C++, utilizamos el operador de comparación "==" que compara si dos valores son iguales. La clase "tm" de la biblioteca estándar de C++ nos permite trabajar con fechas y horas. En el siguiente ejemplo, comparamos dos fechas para ver si son iguales:

```C++
#include <iostream>
#include <ctime>

int main()
{
  // Definimos dos variables de tipo struct tm
  struct tm fecha1 = { .tm_year = 2020, .tm_mon = 11, .tm_mday = 30 };
  struct tm fecha2 = { .tm_year = 2020, .tm_mon = 11, .tm_mday = 30 };

  // Utilizamos el operador de comparación "=="
  if (fecha1 == fecha2) {
    std::cout << "Las fechas son iguales" << std::endl;
  } else {
    std::cout << "Las fechas son diferentes" << std::endl;
  }

  return 0;
}
```

El resultado de este código será "Las fechas son iguales", ya que ambas variables tienen la misma fecha asignada. Ahora, si queremos comparar si una fecha es mayor o menor que otra, podemos utilizar el operador de comparación ">" y "<" respectivamente.

```C++
// Utilizamos el operador de comparación ">"
if (fecha1 > fecha2) {
  std::cout << "fecha1 es mayor que fecha2" << std::endl;
} else {
  std::cout << "fecha1 es menor que fecha2" << std::endl;
}
```

Además de estos operadores de comparación, también es posible utilizar la función "difftime" de la biblioteca estándar de C++, que compara dos fechas y devuelve la diferencia en segundos. Esto puede ser útil si necesitas calcular la edad de una persona o la duración entre dos eventos.

Para utilizar "difftime", debemos convertir nuestras fechas a un formato que la función reconozca. En este caso, utilizaremos la función "mktime" para convertir las fechas a un valor de tipo "time_t", que es el formato aceptado por "difftime".

```C++
// Convertimos las fechas a tipo "time_t"
time_t date1 = mktime(&fecha1);
time_t date2 = mktime(&fecha2);

// Utilizamos la función "difftime" para obtener la diferencia en segundos
double diferencia = difftime(date1, date2);

std::cout << "La diferencia en segundos es de: " << diferencia << std::endl;
```

## Descubriendo Más
Para profundizar en el tema de comparación de fechas en C++, es importante conocer la estructura de datos "struct tm" y sus diferentes campos, como año, mes, día, hora, entre otros. También es necesario tener conocimiento de cómo funcionan las funciones "mktime" y "difftime" para poder utilizarlas correctamente.

Otra opción para trabajar con fechas en C++ es utilizar una librería externa como Boost Date Time Library, que ofrece una amplia variedad de funciones y estructuras para trabajar con fechas y horas de manera más eficiente.

## Ver También
- [Cómo trabajar con la fecha y hora en C++](https://loopback.io/doc/en/lb3/Working-with-Dates.html)
- [Boost Date Time Library](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
- [Estructura de datos "struct tm"](https://www.cplusplus.com/reference/ctime/tm/)