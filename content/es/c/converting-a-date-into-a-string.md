---
title:                "C: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Imagínate que tienes un programa en C que necesita mostrar la fecha actual en una interfaz de usuario. Sin embargo, la función `time()` de C devuelve la fecha en un formato numérico. ¿Cómo podemos convertir esta fecha en una cadena legible para los usuarios? ¡Aquí es donde entra en juego la conversión de fechas en cadenas!

## Cómo hacerlo

Para convertir una fecha en una cadena en C, necesitamos usar la función `strftime()` de la biblioteca estándar `time.h`. Esta función toma tres argumentos: una cadena de formato, una estructura `tm` y el tamaño de la cadena de destino. Aquí hay un ejemplo de cómo usar `strftime()` para obtener la fecha actual en formato de cadena:

```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t now = time(NULL);
  struct tm *current_time = localtime(&now);
  char date_string[20];
  strftime(date_string, 20, "%d/%m/%Y", current_time);
  printf("Hoy es: %s", date_string);
  return 0;
}
```

La salida de este programa sería: `Hoy es: 07/10/2021`.

## Profundizando

La función `strftime()` utiliza especificadores de formato para indicar cómo se debe formatear la fecha. Por ejemplo, en el ejemplo anterior, el especificador `%d` indica el día del mes, `%m` el mes y `%Y` el año. Puedes encontrar una lista completa de especificadores de formato en la documentación de `strftime()`.

También es importante mencionar que la función `localtime()` convierte la fecha en un formato legible para los humanos, que es la estructura `tm`. Esta estructura contiene campos para el año, mes, día, hora, minuto, segundo y más. Por lo tanto, si necesitas obtener alguna información específica de la fecha, puedes acceder a ella a través de la estructura `tm`.

¡Con esta información, ya puedes comenzar a convertir fechas en cadenas en tus programas en C!

## Ver también

- Documentación de `strftime()` en la biblioteca de C: https://www.cplusplus.com/reference/ctime/strftime/
- Información sobre estructuras `tm`: https://www.cplusplus.com/reference/ctime/tm/