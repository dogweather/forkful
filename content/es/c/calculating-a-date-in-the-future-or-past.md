---
title:    "C: Calculando una fecha en el futuro o pasado"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por qué

Calcular una fecha en el futuro o en el pasado puede ser útil en muchas situaciones, como por ejemplo planificar fechas de entrega, programar eventos, o simplemente por curiosidad.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado, primero debemos obtener la fecha actual usando la función `time()` de la biblioteca `time.h`. Luego, podemos utilizar la función `localtime()` para obtener una estructura de tiempo con la fecha actual. A partir de esta estructura de tiempo, podemos modificar los valores de día, mes y año para obtener la fecha deseada.

Veamos un ejemplo de código que calcula la fecha dentro de 1 mes a partir de la fecha actual:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t tiempo_actual = time(NULL); // Obtener el tiempo actual en segundos
    struct tm *fecha_actual = localtime(&tiempo_actual); // Obtener la estructura de tiempo con la fecha actual

    // Modificar los valores de día, mes y año para obtener la fecha dentro de 1 mes
    fecha_actual->tm_mon = fecha_actual->tm_mon + 1;
    
    // Convertir la estructura de tiempo modificada a una fecha válida
    mktime(fecha_actual);

    // Imprimir la fecha en el formato deseado
    printf("La fecha dentro de 1 mes será %d/%d/%d.\n", fecha_actual->tm_mday, fecha_actual->tm_mon + 1, fecha_actual->tm_year + 1900);
    
    return 0;
}
```

La salida del programa sería la siguiente:

```
La fecha dentro de 1 mes será 28/9/2021.
```

## Profundizando

La función `mktime()` utilizada en el ejemplo anterior es una función avanzada que convierte una estructura de tiempo a una fecha válida. Esta función tiene en cuenta el calendario gregoriano y automáticamente ajusta los valores de día, mes y año si se exceden los límites (por ejemplo, si incrementamos el mes en 1 pero el año solo tiene 12 meses).

Además, para obtener una fecha en el pasado, simplemente debemos restar los valores de día, mes y año en lugar de sumarlos.

También podemos calcular una fecha en un intervalo de tiempo específico, por ejemplo, dentro de 3 años a partir de la fecha actual. Para ello, podemos utilizar la función `difftime()` para obtener la diferencia en segundos entre dos fechas y luego utilizar esta diferencia como parámetro en la función `mktime()`.

¡Las posibilidades son infinitas y solo dependen de nuestra imaginación y nuestro código!

## Ver también

- [Documentación de la función `localtime()`](https://www.cplusplus.com/reference/ctime/localtime/)
- [Documentación de la función `mktime()`](https://www.cplusplus.com/reference/ctime/mktime/)
- [Documentación de la función `difftime()`](https://www.cplusplus.com/reference/ctime/difftime/)