---
title:                "Obteniendo la fecha actual"
html_title:           "C++: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Obtener la fecha actual es una tarea común en la programación, ya sea para mostrar la fecha actual en una aplicación o para realizar cálculos basados ​​en ella. En esta sección, aprenderás cómo obtener la fecha actual en C++ de manera fácil y eficiente.

## Cómo hacerlo

Para obtener la fecha actual en C++, necesitamos incluir la librería `ctime`, que proporciona las funciones necesarias para trabajar con fechas y horas.

Dentro de nuestro código, podemos usar la función `time()` que devuelve un valor de tipo `time_t` que representa el número de segundos desde el 1 de enero de 1970. Luego, podemos pasar este valor a la función `localtime()` para obtener una estructura de tipo `tm` con la fecha y hora actual.

Veamos un ejemplo con el código:

```C++
#include <iostream>
#include <ctime>

int main() {
    // Obtener la fecha y hora actual
    time_t now = time(0);
    tm *today = localtime(&now);

    // Imprimir la fecha en formato dd/mm/yyyy
    std::cout << "Hoy es " << today->tm_mday << "/" << (today->tm_mon + 1) << "/" << (today->tm_year + 1900) << std::endl;

    // Imprimir la hora en formato hh:mm:ss
    std::cout << "Son las " << today->tm_hour << ":" << today->tm_min << ":" << today->tm_sec << std::endl;

   return 0;
}
```

**Salida:**

```
Hoy es 3/5/2021
Son las 18:30:00
```

## Profundizando

La función `localtime()` utiliza la zona horaria del sistema y devuelve la hora local. Sin embargo, si queremos obtener la hora local con un desplazamiento específico, podemos usar la función `localtime_s()` que nos permite especificar este desplazamiento.

Algunas otras funciones útiles relacionadas con fechas y horas en C++ son:

- `gmtime()` - obtiene la fecha y hora UTC actual.
- `asctime()` - convierte una estructura de tipo `tm` en una cadena de caracteres.
- `mktime()` - convierte una estructura de tipo `tm` en el número de segundos desde el 1 de enero de 1970.

¡Ahora estás listo para trabajar con fechas y horas en C++!

## Ver también

- [Cómo trabajar con cadenas de caracteres en C++](https://www.ejemplo.com/cadenas-de-caracteres-c++)
- [Introducción a la utilización de librerías en C++](https://www.ejemplo.com/librerias-c++)