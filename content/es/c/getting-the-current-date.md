---
title:                "Obteniendo la fecha actual"
html_title:           "C: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Existen muchas razones por las que uno querría obtener la fecha actual en un programa en C. Quizás necesitas mostrar la fecha en un formato específico a un usuario, o tal vez necesitas realizar cálculos basados en la fecha. Sea cual sea el motivo, es importante saber cómo obtener la fecha actual en un programa.

## Cómo hacerlo

Para obtener la fecha actual en un programa en C, necesitas utilizar la función `time()` de la biblioteca estándar `time.h`. Esta función devolverá un valor que representa el número de segundos transcurridos desde la medianoche del 1 de enero de 1970. A partir de ese valor, puedes usar la función `localtime()` para convertirlo en una estructura de tipo `struct tm` que contiene información detallada sobre la fecha y hora actual. Aquí hay un ejemplo de cómo hacerlo:

```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t seconds = time(NULL);
  
  struct tm *actual = localtime(&seconds);

  printf("La fecha actual es: %d/%d/%d\n", actual->tm_mday, actual->tm_mon + 1, actual->tm_year + 1900);
  printf("La hora actual es: %d:%d:%d\n", actual->tm_hour, actual->tm_min, actual->tm_sec);

  return 0;
}
```

Output:

```
La fecha actual es: 16/10/2020
La hora actual es: 12:30:00
```

## Deep Dive

Como se mencionó anteriormente, la función `time()` devuelve el número de segundos transcurridos desde la medianoche del 1 de enero de 1970. Esto se conoce como "epoch time" y es una convención utilizada en muchos sistemas operativos para representar el tiempo. Luego, la función `localtime()` toma ese valor y lo convierte en una estructura que contiene información sobre la fecha y hora actual en la zona horaria local del sistema. Sin embargo, si necesitas trabajar con fechas y horas en otras zonas horarias, puedes utilizar la función `gmtime()` que devuelve la fecha y hora en UTC (tiempo universal coordinado). También hay otras funciones disponibles en la biblioteca `time.h` para realizar cálculos con fechas y horas, como `difftime()` que calcula la diferencia entre dos momentos en segundos.

## Ver también

- [Más información sobre la función `time()` y la estructura `struct tm`](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Ejemplos de uso de funciones relacionadas con la fecha y hora en C](https://www.programiz.com/c-programming/examples/current-date-time)
- [Información sobre el "epoch time"](https://www.epochconverter.com/)