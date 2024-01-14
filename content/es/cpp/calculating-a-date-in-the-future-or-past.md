---
title:    "C++: Calculando una fecha en el futuro o en el pasado"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Calcular la fecha en el futuro o en el pasado puede ser útil en una variedad de situaciones, como planificar eventos o verificar la edad de una persona en una fecha específica. En este blog post, aprenderemos cómo realizar esta tarea en C++.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado, necesitaremos tres piezas de información: la fecha actual, el número de días que queremos sumar o restar y la fecha resultante que queremos calcular.

Primero, necesitamos definir una estructura de datos para almacenar la fecha, que constará de tres variables: día, mes y año.

```C++
struct Fecha {
  int dia;
  int mes;
  int año;
};
```

A continuación, crearemos una función que tome una fecha como entrada y devuelva la misma fecha actualizada con los días especificados. Si queremos calcular una fecha en el futuro, sumaremos los días al día actual. Si queremos calcular una fecha en el pasado, restaremos los días al día actual.

```C++
Fecha calcularFecha(int dias, Fecha fechaActual) {
  fechaActual.dia += dias;
  return fechaActual;
}
```

Finalmente, podemos llamar a esta función y asignar la fecha resultante a una variable nueva. Por ejemplo, si queremos calcular la fecha 100 días después del 1 de enero de 2021, podemos hacerlo de la siguiente manera:

```C++
Fecha fechaFutura = calcularFecha(100, {1, 1, 2021});
```

El resultado sería una estructura de datos con la fecha del día 11 de abril de 2021.

## Profundizando

En nuestro ejemplo anterior, utilizamos una estructura de datos para almacenar la fecha. En su lugar, también podemos utilizar la biblioteca de fecha y hora de C++ para trabajar con objetos de fecha y hora. Esto nos permitiría realizar operaciones más complejas, como comparar fechas o calcular la diferencia entre dos fechas.

También podemos ampliar nuestra función para incluir diferentes unidades de tiempo, como semanas o meses, en lugar de solo días.

## Ver también

- [Documentación oficial de C++ sobre fechas y horas](https://en.cppreference.com/w/cpp/chrono)
- [Calculando el día de la semana en C++](https://www.geeksforgeeks.org/find-day-of-the-week-for-a-given-date/)
- [Usando la biblioteca Boost para trabajar con fechas en C++](https://www.boost.org/doc/libs/1_70_0/doc/html/date_time.html)