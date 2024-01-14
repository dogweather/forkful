---
title:                "C++: Comparando dos fechas"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué comparar dos fechas en C++

Así como en cualquier otro lenguaje de programación, comparar fechas en C++ es una tarea común y muy útil. Puede ser necesario para realizar operaciones matemáticas, ordenar datos o simplemente para comprobar si una fecha es mayor o menor que otra. En este artículo, exploraremos cómo realizar esta tarea de manera efectiva en C++.

## Cómo hacerlo

Para comparar dos fechas en C++, es necesario crear dos objetos de la clase `tm` que representen las fechas que deseamos comparar. La clase `tm` es una estructura de datos estándar de C++ que contiene información sobre fecha y hora. A continuación, se muestra un ejemplo de cómo crear dos objetos `tm` con fechas diferentes:

```C++
tm date1 = {0, 0, 0,  //segundos, minutos, horas
            1, 2, 2021};  //día, mes, año
tm date2 = {0, 0, 0,
            3, 4, 2022};
```

Una vez que tenemos nuestros dos objetos `tm`, podemos utilizar la función `difftime()` de C++ para calcular la diferencia en segundos entre las dos fechas. Esta función toma dos argumentos: las dos fechas a comparar en forma de punteros a objetos `tm`. A continuación, se muestra cómo calcular la diferencia entre `date2` y `date1`:

```C++
double diff = difftime(&date2, &date1);
```

El resultado de esta operación será un número en segundos, que puede ser utilizado para realizar comparaciones entre las dos fechas.

Ahora que tenemos la diferencia en segundos, podemos realizar diversas operaciones, como por ejemplo, comprobar si una fecha es anterior o posterior a otra, o calcular cuántos días hay entre dos fechas. A continuación, se muestran algunos ejemplos de cómo utilizar la diferencia en segundos:

```C++
if (diff > 0) {
    // date2 es posterior a date1
} else if (diff < 0) {
    // date2 es anterior a date1
} else {
    // las fechas son iguales
}
```

```C++
int days = int(diff / (24*60*60)); // obtener la diferencia en días
cout << "Entre date1 y date2 hay " << days << " días de diferencia." << endl;
```

```C++
date1.tm_hour += int(s); // sumar la diferencia en segundos a la hora de date1
mktime(&date1); // actualizar la fecha teniendo en cuenta la nueva hora
```

## Más información sobre comparar fechas

Además de la función `difftime()`, C++ ofrece otras funciones útiles para trabajar con fechas, como `mktime()`, `gmtime()` y `localtime()`. Estas funciones permiten convertir una fecha a diferentes formatos, como fechas locales o en formato de hora GMT.

En la mayoría de los casos, solo necesitarás utilizar la función `difftime()` para realizar operaciones con fechas en C++. Sin embargo, si estás trabajando con fechas en un formato específico, estas otras funciones pueden ser de ayuda.

## Ver también

- [Documentación de la clase `tm` en C++](https://www.cplusplus.com/reference/ctime/tm/)
- [Guía de programación de fechas y horas en C++](https://www.gnu.org/software/libc/manual/html_node/Date-and-Time.html)
- [Ejemplos de código de comparación de fechas en C++](https://www.geeksforgeeks.org/comparing-two-dates-c-cpp/)

Gracias por leer este artículo sobre cómo comparar dos fechas en C++. Esperamos que sea de ayuda para tu desarrollo como programador. ¡Hasta la próxima!