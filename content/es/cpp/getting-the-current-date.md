---
title:                "C++: Obteniendo la fecha actual"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Por qué obtener la fecha actual es útil

Obtener la fecha actual en un programa de C++ es una tarea fundamental para llevar a cabo diversas funciones. Ya sea para imprimir la fecha en un archivo, realizar cálculos de tiempo o simplemente mostrar la fecha al usuario, tener acceso a la fecha actual es una habilidad importante para cualquier programador.

##Cómo obtener la fecha actual en C++

Para obtener la fecha actual, debemos utilizar la biblioteca "ctime" de C++. Esta biblioteca proporciona funciones para trabajar con datos de tiempo y fecha. Primero, debemos incluir la biblioteca en nuestro programa:

```C++
#include <ctime>
```

Luego, podemos utilizar la función "time" para obtener la fecha actual en segundos desde la época de Unix (1 de enero de 1970):

```C++
time_t now = time(0);
```

Para convertir el tiempo en un formato legible, podemos utilizar la función "localtime" para obtener una estructura de tiempo que contenga los componentes separados de la fecha actual:

```C++
tm *ltm = localtime(&now);
```

Finalmente, podemos imprimir la fecha en el formato deseado utilizando los componentes de la estructura de tiempo:

```C++
cout << "La fecha actual es: " << ltm->tm_mon + 1 << "/" << ltm->tm_mday << "/" << ltm->tm_year + 1900 << endl;
```

El resultado de este código sería:

```C++
La fecha actual es: 3/12/2021
```

##Profundizando en la obtención de la fecha actual

Para obtener una mejor comprensión de cómo funciona el proceso de obtener la fecha actual en C++, podemos analizar la estructura de tiempo utilizada y sus componentes. Los componentes de la estructura de tiempo son:

- tm_sec: segundos (0-59)
- tm_min: minutos (0-59)
- tm_hour: horas (0-23)
- tm_mday: día del mes (1-31)
- tm_mon: mes (0-11)
- tm_year: año desde 1900
- tm_wday: día de la semana (0-6)
- tm_yday: día del año (0-365)
- tm_isdst: indicador de horario de verano (1 si está en horario de verano, 0 de lo contrario)

También podemos utilizar otras funciones de la biblioteca "ctime" para trabajar con fechas y horas, como "mktime" para convertir una estructura de tiempo en una fecha y hora válidas, o "strftime" para formatear la fecha y hora de acuerdo con un patrón específico.

##Ver también

- [Documentación de la biblioteca <ctime> en cplusplus.com](http://www.cplusplus.com/reference/ctime/)
- [Tutorial sobre la biblioteca <ctime> en cppreference.com](https://en.cppreference.com/w/cpp/chrono/c/strftime)
- [Códigos fuente de ejemplo para obtener la fecha actual en C++ de Geeks for Geeks](https://www.geeksforgeeks.org/cpp-program-print-current-day-month-year/)