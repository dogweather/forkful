---
title:                "C: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Obtener la fecha actual es una tarea común en la programación C. Esto puede ser útil para muchas aplicaciones, como registros de eventos, seguimiento de cronogramas y más. Al tener la fecha actual, podemos realizar diversas operaciones utilizando la información de fecha y hora.

## Cómo hacerlo

Para obtener la fecha actual en C, utilizamos la función `time()` de la biblioteca `time.h`. Esta función devuelve la cantidad de segundos desde el 1 de enero de 1970 a las 00:00:00 GMT. Esto se conoce como el "epoch time". Para utilizar esta función, primero debemos incluir la biblioteca `time.h` en nuestro programa.

```C
// Incluir la biblioteca time.h
#include <time.h>

int main(){

    // Declarar una variable de tipo time_t para almacenar la fecha actual
    time_t time_current;

    // Obtener la fecha actual usando la función time()
    time(&time_current);

    // Imprimir la fecha actual en formato de número entero
    printf("La fecha actual es: %ld", time_current);

    return 0;
}
```

La salida del programa sería algo como:

```
La fecha actual es: 1614732556
```

Podemos ver que obtenemos un número entero grande, que representa la cantidad de segundos desde el 1 de enero de 1970. Sin embargo, este número no es útil para la mayoría de los usuarios. Por lo tanto, necesitamos convertirlo a un formato de fecha y hora legible.

Podemos hacer eso utilizando la función `ctime()`. Esta función convierte la fecha y hora en un formato de cadena de caracteres leíble para humanos. También debemos tener en cuenta que la función `ctime()` agrega un salto de línea al final, por lo que se debe tener en cuenta al imprimir la salida.

```C
// Incluir la biblioteca time.h
#include <time.h>

int main(){

    // Declarar una variable de tipo time_t para almacenar la fecha actual
    time_t time_current;

    // Obtener la fecha actual usando la función time()
    time(&time_current);

    // Convertir la fecha actual en una cadena de caracteres legíble
    char *date_current = ctime(&time_current);

    // Imprimir la fecha actual
    printf("La fecha actual es: %s", date_current);

    return 0;
}
```

La salida del programa sería:

```
La fecha actual es: Wed Mar 03 23:55:56 2021
```

¡Ahora obtenemos una fecha y hora legíble!

## Deep Dive

Introducir la función `time()` y la biblioteca `time.h` puede llevar a pensar que solo obtendremos la fecha actual. Sin embargo, podemos realizar otras operaciones usando esta información. Por ejemplo, podemos utilizar la estructura `tm` de la biblioteca `time.h` para separar la fecha y hora en diferentes componentes como año, mes, día, hora, minuto, segundo, entre otros.

Además, podemos utilizar otras funciones, como `localtime()` para obtener la fecha y hora local, `gmtime()` para obtener la fecha y hora GMT, `strftime()` para formatear la salida, entre otras.

En resumen, obtener la fecha actual en C puede ser una tarea sencilla, pero también es importante conocer las diferentes herramientas que nos permiten manipular y utilizar esta información de manera más eficiente.

## Ver también

- [Documentación de la función time() en cplusplus.com](http://www.cplusplus.com/reference/ctime/time/)
- [Tutorial sobre trabajar con fechas y horas en C](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)
- [Documentación de la biblioteca time.h en la página oficial de GNU](https://www.gnu.org/software/libc/manual/html_node/Time-of-Day.html)