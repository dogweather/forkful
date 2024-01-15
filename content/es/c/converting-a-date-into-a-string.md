---
title:                "Convirtiendo una fecha en una cadena."
html_title:           "C: Convirtiendo una fecha en una cadena."
simple_title:         "Convirtiendo una fecha en una cadena."
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué?

A menudo, en programación, necesitamos mostrar una fecha en formato de texto para que sea más fácil de leer para el usuario. Por ejemplo, en una aplicación de calendario, queremos que las fechas aparezcan como "Lunes 27 de septiembre de 2021" en lugar de "27/09/2021". En tales casos, es útil saber cómo convertir una fecha en una cadena de texto en C.

## ¿Cómo hacerlo?

Para convertir una fecha en una cadena de texto en C, necesitamos utilizar la función `strftime()` de la librería `time.h`. Esta función toma tres argumentos: la cadena de texto en la cual queremos que se guarde la fecha, el tamaño máximo de la cadena y un formato especificando cómo deseamos que se muestre la fecha.

Veamos un ejemplo:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t tiempo = time(NULL);
    char fecha[20];
    strftime(fecha, 20, "%A %d de %B de %Y", localtime(&tiempo));
    printf("Hoy es %s.", fecha);
    return 0;
}
```
La salida de este código sería:
```
Hoy es lunes 27 de septiembre de 2021.
```
Veamos en detalle qué está sucediendo en este código:
1. En la primera línea, incluimos las librerías `stdio.h` y `time.h` que nos permitirán utilizar las funciones que necesitamos.
2. Luego, en la línea 4, declaramos una variable de tipo `time_t` llamada `tiempo` y la inicializamos con la función `time(NULL)`. Esta función nos devuelve la fecha y hora actuales en forma de segundos desde el 1 de enero de 1970.
3. En la línea 5, declaramos un array de caracteres llamado `fecha` con tamaño suficiente para almacenar la fecha en formato de texto.
4. A continuación, en la línea 6, utilizamos la función `strftime()` para convertir la fecha en una cadena de texto. Le pasamos como argumentos la cadena `fecha`, el tamaño máximo de la cadena (20 caracteres) y el formato que deseamos que tenga la fecha (en este caso, "Día de la semana Día de mes de Año").
5. Por último, en la línea 7, imprimimos la cadena de texto en la que se ha almacenado la fecha utilizando la función `printf()`.

## Profundizando

La función `strftime()` nos permite personalizar el formato de la fecha según nuestras necesidades, utilizando una serie de especificadores de formato. Algunos de los más comunes son los siguientes:

- `%A`: día de la semana completo (por ejemplo, "lunes")
- `%a`: día de la semana abreviado (por ejemplo, "lun")
- `%B`: mes completo (por ejemplo, "septiembre")
- `%b`: mes abreviado (por ejemplo, "sep")
- `%d`: día del mes (por ejemplo, "27")
- `%Y`: año con cuatro dígitos (por ejemplo, "2021")
- `%y`: año con dos dígitos (por ejemplo, "21")

Puedes encontrar una lista completa de los especificadores de formato en la documentación de `strftime()`. Además, existen otras funciones como `strptime()` y `asctime()` que también permiten convertir fechas en cadenas de texto de diferentes formas.

## Ver también

- Documentación de `strftime()`: https://www.opennet.ru/man.shtml?topic=strftime&category=3&russian=0
- Tutorial de la librería `time.h`: https://www.tutorialspoint.com/c_standard_library/time_h.htm
- Ejemplos de uso de `strftime()`: https://www.ibm.com/support/knowledgecenter/SSLTBW_2.3.0/com.ibm.zos.v2r3.bpxbd00/strt.htm