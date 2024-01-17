---
title:                "Obteniendo la fecha actual."
html_title:           "C: Obteniendo la fecha actual."
simple_title:         "Obteniendo la fecha actual."
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Obtener la fecha actual en un programa puede ser útil para mostrar la fecha y hora de cuando se ejecutó el programa o para realizar operaciones basadas en la fecha actual. Los programadores pueden utilizar esta función para monitorear el tiempo de ejecución de un programa o para realizar cálculos con fechas.

## ¿Cómo hacerlo?
En C, podemos obtener la fecha actual utilizando la función `time()` de la biblioteca estándar `time.h`. Primero, debemos incluir esta biblioteca en nuestro código. Luego, podemos llamar a la función `time()` y almacenar el valor devuelto en una variable de tipo `time_t`. A continuación, podemos utilizar la función `localtime()` para convertir este valor en una estructura `tm` que contiene información sobre la fecha y hora actual. Aquí hay un ejemplo de código:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Obtenemos la fecha actual
  time_t now = time(NULL);
  
  // Convertimos el valor en una estructura tm
  struct tm *current_date = localtime(&now);
  
  // Imprimimos la fecha actual
  printf("La fecha actual es: %d/%d/%d\n", current_date->tm_mday, current_date->tm_mon + 1, current_date->tm_year + 1900);
  
  return 0;
}
```

Este es un ejemplo de salida:

```
La fecha actual es: 21/11/2020
```

## Detalles más profundos
La función `time()` fue introducida en el lenguaje C en la versión C89. Antes de eso, los programadores tenían que utilizar funciones específicas para el sistema operativo en el que estaban trabajando para obtener la fecha actual. Sin embargo, estas funciones eran específicas de cada sistema operativo y no eran portables.

Además de utilizar la función `localtime()`, también podemos utilizar la función `gmtime()` para obtener la fecha y hora en formato UTC (Tiempo Universal Coordinado). Otra alternativa es utilizar la función `strftime()` para formatear la fecha y hora de diferentes maneras. Consulte la documentación de la biblioteca `time.h` para obtener más detalles.

## Ver también
- [Documentación de la función time() en cppreference.com](https://es.cppreference.com/w/c/chrono/clock)
- [Más información sobre la estructura tm en cplusplus.com](http://www.cplusplus.com/reference/ctime/tm/)