---
title:    "C: Obteniendo la fecha actual"
keywords: ["C"]
---

{{< edit_this_page >}}

# ¿Por qué obtener la fecha actual en programación?

Muchas veces en la programación, necesitamos trabajar con fechas para realizar diferentes tareas. Esto puede ser desde mostrar la fecha en un programa hasta realizar cálculos basados en fechas. En este artículo, te enseñaremos cómo obtener la fecha actual en C y cómo puedes usar esta información en tus proyectos.

## Cómo obtener la fecha actual

Para obtener la fecha actual en C, debemos usar la función `time()`. Esta función se encuentra en la biblioteca estándar de C `time.h` y nos devuelve la cantidad de segundos transcurridos desde el 1 de enero de 1970 a las 00:00 am UTC. Luego, podemos convertir esta cantidad de segundos a una estructura `tm` que contiene información sobre la fecha y hora actual.

```
#include <stdio.h>
#include <time.h>

int main() {
    // Obtenemos el tiempo actual en segundos
    time_t tiempo_actual = time(NULL);

    // Convertimos los segundos a la estructura tm
    struct tm *fecha_actual = localtime(&tiempo_actual);
    
    // Imprimimos la fecha y hora actual en formato dd/mm/yyyy
    printf("La fecha actual es: %02d/%02d/%d\n", fecha_actual->tm_mday, fecha_actual->tm_mon + 1, fecha_actual->tm_year + 1900);

    return 0;
}
```

El output de este código sería:

```
La fecha actual es: 09/12/2021
```

## Inmersión en la obtención de la fecha actual

Ahora que sabemos cómo obtener la fecha actual, podemos profundizar un poco más en cómo funciona el proceso. Como mencionamos anteriormente, la función `time()` nos devuelve la cantidad de segundos transcurridos desde una fecha de referencia. Esto se conoce como "epoch time" y es una forma común de medir el tiempo en sistemas informáticos.

Una vez que tenemos esta cantidad de segundos, podemos utilizar la función `localtime()` para convertirlos en una estructura `tm` que contiene las siguientes variables:

- `tm_sec`: segundos desde el minuto (0-60).
- `tm_min`: minutos desde la hora (0-59).
- `tm_hour`: horas desde la medianoche (0-23).
- `tm_mday`: día del mes (1-31).
- `tm_mon`: meses desde enero (0-11).
- `tm_year`: años desde 1900.
- `tm_wday`: días desde domingo (0-6).
- `tm_yday`: días desde enero 1 (0-365).
- `tm_isdst`: horario de verano (1 si está en horario de verano, 0 si no, -1 si es desconocido).

De esta forma, podemos acceder a cada una de estas variables para obtener la información que necesitamos sobre la fecha y hora actual.

## Ver también

- [Documentación de la función time() en C](https://www.cplusplus.com/reference/ctime/time/)
- [Tutorial de C en español](https://www.tutorialspoint.com/cprogramming/index.htm)