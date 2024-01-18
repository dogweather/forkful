---
title:                "Extrayendo una fecha de una cadena."
html_title:           "C: Extrayendo una fecha de una cadena."
simple_title:         "Extrayendo una fecha de una cadena."
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

El análisis de una fecha a partir de una cadena de texto es un proceso que permite a los programadores extraer información de una fecha dada en forma de texto. Esto es útil para manipular y trabajar con fechas en formato de texto, ya sea para mostrarlas al usuario o realizar cálculos con ellas en el código.

## Cómo hacerlo:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
  char fecha[] = "10/05/2021";
  struct tm tm;
  strptime(fecha, "%d/%m/%Y", &tm);
  printf("El día de la semana es: %d\n", tm.tm_wday);
  printf("El mes es: %d\n", tm.tm_mon + 1);
  printf("El día del mes es: %d\n", tm.tm_mday);
  printf("El año es: %d\n", tm.tm_year + 1900);

  return 0;
}
```

Salida:
```
El día de la semana es: 1
El mes es: 5
El día del mes es: 10
El año es: 2021
```

## Profundizando

El análisis de fechas es una tarea común en la programación, especialmente en aplicaciones que manejan información sobre eventos y horarios. Algunos lenguajes de programación, como Python o JavaScript, tienen funciones integradas para analizar fechas, como `datetime.strptime()` y `Date.parse()`. En C, se puede utilizar la función `strptime()` de la librería `<time.h>` para realizar la tarea.

Es importante tener en cuenta que el análisis de fechas puede variar dependiendo del formato en que se encuentra la fecha en la cadena de texto. Es necesario especificar cómo está estructurada la fecha en la cadena mediante la utilización de especificadores de formato, como `%d` para el día, `%m` para el mes y `%Y` para el año.

## Ver También

- Documentación oficial de la función strptime de C: https://www.cplusplus.com/reference/ctime/strptime/ 
- Tutorial de análisis y manipulación de fechas en C: https://www.tutorialspoint.com/c_standard_library/time_h.htm