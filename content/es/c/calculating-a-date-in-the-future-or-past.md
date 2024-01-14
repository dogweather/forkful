---
title:    "C: Calculando una fecha en el futuro o el pasado"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Por qué

Hay muchas razones por las cuales alguien podría querer calcular una fecha en el futuro o en el pasado en un programa de C. Puede ser para planificar eventos, realizar cálculos financieros o simplemente por curiosidad. Independientemente de la razón, saber cómo hacerlo puede ser muy útil en muchas situaciones.

# Cómo

Para calcular una fecha en el futuro o en el pasado en un programa de C, utilizaremos la función `mktime()` de la biblioteca estándar `time.h`. Esta función toma como argumentos la estructura de fecha y hora `tm` y devuelve un valor de tipo `time_t`. Para evitar errores, es importante asegurarse de utilizar valores correctos para los diferentes campos de la estructura `tm`. Por ejemplo, si queremos calcular la fecha 1 de enero de 2022, deberemos asignar 1 al campo `tm_mday`, 0 al campo `tm_mon` (ya que los meses comienzan desde 0) y 122 (2022-1900) al campo `tm_year`. A continuación, un ejemplo de código que calcula una fecha en el futuro de un año a partir de la fecha actual:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Obtener fecha y hora actual
    time_t now = time(NULL);
    
    // Convertir a estructura de tm
    struct tm *current_time = localtime(&now);
    
    // Cambiar año a 2022
    current_time -> tm_year = 122;
    
    // Calcular fecha en el futuro
    time_t future = mktime(current_time);

    // Imprimir fecha futura
    printf("Fecha dentro de un año: %s", ctime(&future));

    return 0;
}
```

La salida de este programa sería:

```
Fecha dentro de un año: Sun Jan  1 00:00:00 2023
```

# Profundizando

Además de la función `mktime()`, existen otras funciones útiles para trabajar con fechas en C, como `localtime()`, que convierte un valor `time_t` en una estructura `tm` en la hora local, o `strftime()`, que permite formatear una fecha en una cadena de caracteres. También es importante tener en cuenta que `time_t` representa el tiempo en segundos desde la época (1 de enero de 1970 en sistemas UNIX), por lo que si necesitamos trabajar con fechas anteriores a esa fecha, deberemos utilizar una biblioteca externa o realizar cálculos adicionales.

# Ver también

- [Documentación de la función `mktime()` en C](https://www.cplusplus.com/reference/ctime/mktime/)
- [Tutorial sobre fechas en C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Biblioteca externa para cálculos de fechas: `libtai`](http://cr.yp.to/libtai.html)