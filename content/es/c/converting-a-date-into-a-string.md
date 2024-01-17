---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "C: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Cuando trabajas con fechas en un programa, es posible que necesites convertirlas en formato de texto para funciones específicas. Esto se conoce como "convertir una fecha en una cadena". Los programadores lo hacen para manejar las fechas de forma más eficiente y para mostrarlas en una forma legible para los usuarios.

## Cómo:
El lenguaje C tiene varias formas de convertir una fecha en una cadena. Una forma común es usando la función `strftime`, que toma como argumentos: el formato deseado de la cadena, la fecha y la hora en la que estamos interesados y una variable para almacenar la cadena resultante. A continuación, se muestra un ejemplo de cómo usarla:

```
#include <stdio.h>
#include <time.h>

int main() {
   char fecha_actual[20];
   time_t ahora;
   struct tm *tiempo;
   ahora = time(0);
   tiempo = localtime(&ahora);
   // Convertir la fecha en una cadena con el formato YYYY-MM-DD
   strftime(fecha_actual, 20, "%Y-%m-%d", tiempo);
   printf("Hoy es %s.", fecha_actual);
   return 0;
}
```
Output:
```
Hoy es 2021-07-01.
```

## Profundizando:
Existen otras formas de convertir una fecha en una cadena en C, como usar la función `sprintf` o manipular manualmente los componentes de la estructura `tm` que almacena la fecha y hora. Sin embargo, `strftime` es la opción más común y más fácil de entender.

En cuanto a alternativas, existen diferentes bibliotecas externas que ofrecen funcionalidad adicional para manejar fechas en C, como `libcdate` o `libtomdate`. Estas pueden ser útiles si necesitas funciones de formato de fecha más avanzadas.

En cuanto a detalles de implementación, la estructura `tm` almacenada por `strftime` utiliza valores enteros para representar componentes de fecha y hora, y su definición puede variar según la plataforma en la que estés trabajando. Por lo tanto, es importante revisar la documentación del lenguaje antes de usar esta función.

## Ver también:
- [Documentación oficial de strftime en C](https://www.cplusplus.com/reference/ctime/strftime/)
- [Alternativas a strftime en C](https://www.codeproject.com/articles/15879/libcdate-a-date-manipulation-library-for-c)
- [Más información sobre estructuras tm en C](https://codeforwin.org/2018/05/tm-structure-programming-examples.html)