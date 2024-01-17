---
title:                "Comparando dos fechas"
html_title:           "C: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Comparar dos fechas es un proceso común en programación que implica verificar si una fecha es igual, anterior o posterior a otra fecha. Los programadores realizan este tipo de comparaciones para realizar diferentes acciones según el resultado obtenido.

## Cómo hacerlo:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Definir dos estructuras tm para representar dos fechas
    struct tm fecha1, fecha2;

    // Asignar valores a las estructuras
    fecha1.tm_year = 2020 - 1900;
    fecha1.tm_mon = 11;
    fecha1.tm_mday = 25;
    fecha1.tm_hour = 0;
    fecha1.tm_min = 0;
    fecha1.tm_sec = 0;

    fecha2.tm_year = 2021 - 1900;
    fecha2.tm_mon = 0;
    fecha2.tm_mday = 1;
    fecha2.tm_hour = 0;
    fecha2.tm_min = 0;
    fecha2.tm_sec = 0;

    // Comparar las fechas utilizando la función difftime de la librería time.h
    double diferencia = difftime(mktime(&fecha2), mktime(&fecha1));

    if (diferencia == 0.0)
        printf("Las fechas son iguales\n");
    else if (diferencia > 0.0)
        printf("La fecha 2 es posterior a la fecha 1\n");
    else
        printf("La fecha 1 es posterior a la fecha 2\n");

    return 0;
}
```

La salida de este programa será: "La fecha 2 es posterior a la fecha 1", ya que la fecha 2 es el 1 de enero de 2021 y la fecha 1 es el 25 de diciembre de 2020.

## Profundizando:

En el pasado, los programadores debían manejar el cálculo de fechas de forma manual, utilizando algoritmos complejos. Sin embargo, con el avance de la tecnología, se crearon librerías y funciones que facilitaron esta tarea. Algunas alternativas a la función difftime son: la función difftime de la librería chrono de C++, la función datediff de la librería datetime de Python y la función DateDiff de SQL.

La función difftime se basa en el concepto de timestamp, que representa la cantidad de segundos transcurridos desde el 1 de enero de 1970. Al restar los timestamps de dos fechas, obtenemos la diferencia en segundos. Luego, esta diferencia se puede convertir a días, meses o años utilizando el factor de conversión adecuado.

## Ver también:

- [Documentación de la función difftime de la librería time.h](https://www.cplusplus.com/reference/ctime/difftime/)
- [Documentación de la función datediff de la librería datetime de Python](https://docs.python.org/3/library/datetime.html#datetime.date.datediff)
- [Documentación de la función DateDiff de SQL](https://docs.microsoft.com/en-us/sql/t-sql/functions/datediff-transact-sql)