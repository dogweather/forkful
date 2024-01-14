---
title:                "C: Obteniendo la fecha actual"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez has querido saber qué día es hoy? O tal vez necesitas obtener la fecha actual para realizar cálculos u otras tareas en tu programa. Obtener la fecha y hora actual es una función esencial en muchos programas y en este artículo te enseñaré cómo hacerlo en lenguaje C.

## Cómo
Para obtener la fecha actual en C, utilizaremos la función `time()` de la librería `time.h`. Primero, la incluiremos en nuestro programa:

```C
#include <time.h>
```

Luego, usaremos la función `time()` para obtener el número de segundos transcurridos desde el 1 de enero de 1970 a las 00:00:00 (también conocido como *epoch*). Este número se conoce como *epoch time* y es utilizado para calcular la fecha y hora actual.

```C
 time_t current_time;
 time(&current_time);
```

Ahora, necesitamos convertir ese número de segundos en una forma legible para los humanos. Para ello, usaremos la función `localtime()` que nos dará una estructura de tipo `tm` con los valores de año, mes, día, hora, minutos y segundos.

```C
struct tm* local_time = localtime(&current_time);
```

Finalmente, podemos imprimir la fecha y hora actual utilizando la función `printf()` y los valores de la estructura `tm`.

```C
printf("La fecha actual es: %d/%d/%d\n", local_time->tm_mday, local_time->tm_mon + 1, local_time->tm_year + 1900);
printf("La hora actual es: %d:%d:%d\n", local_time->tm_hour, local_time->tm_min, local_time->tm_sec);
```

El resultado de este código puede verse así:

```
La fecha actual es: 19/03/2021
La hora actual es: 16:30:00
```

## Profundizando
Como mencionamos anteriormente, el número de segundos desde *epoch* se utiliza para calcular la fecha y hora actual. Pero, ¿cómo lo hace exactamente la función `localtime()`?

La respuesta es que utiliza la información de la zona horaria local (que puede ser modificada por el usuario) y una estructura de datos llamada *time zone database* que contiene información sobre los husos horarios y los cambios en el horario de verano.

Además, vale la pena mencionar que en sistemas operativos basados en Unix, el *epoch* se calcula a partir de la fecha 1 de enero de 1970, mientras que en sistemas basados en Windows, se utiliza la fecha 1 de enero de 1601.

## Ver también
- [Documentación de la función `time()` en C](https://en.cppreference.com/w/cpp/chrono/c/time)
- [Tutorial sobre la librería `time.h`](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Explicación de la estructura `tm` en C](https://www.programiz.com/c-programming/c-structure-tm)
- [Página oficial de la *time zone database*](https://www.iana.org/time-zones)