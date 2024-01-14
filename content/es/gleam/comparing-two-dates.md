---
title:    "Gleam: Comparando dos fechas"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por Qué

En la programación, a menudo necesitamos comparar fechas para realizar diversas operaciones. Esto puede incluir verificar si una fecha es mayor o menor que otra, calcular la diferencia en días o simplemente ordenar una lista de fechas. En esta publicación, exploraremos cómo podemos comparar dos fechas en Gleam y las diferentes funciones y operadores que podemos usar para hacerlo.

## Cómo Hacerlo

Para comenzar, primero necesitamos almacenar nuestras fechas en el formato adecuado. Gleam tiene un tipo de datos especial llamado `Date` que nos permite crear y manipular fechas de forma sencilla. Para crear una fecha, simplemente debemos especificar el año, mes y día. Por ejemplo:

```Gleam
let fecha1 = Date.new(2021, 3, 15)
let fecha2 = Date.new(2021, 3, 20)
```

Ahora que tenemos nuestras fechas, podemos usar diferentes funciones y operadores para compararlas:

- `==`: Compara si dos fechas son iguales.
- `<`: Compara si una fecha es anterior a otra.
- `>`: Compara si una fecha es posterior a otra.
- `difference_in_days()`: Calcula la diferencia en días entre dos fechas.

Por ejemplo, para verificar si `fecha1` es mayor que `fecha2`, podemos usar el operador `>` de la siguiente manera:

```Gleam
let esMayor = fecha1 > fecha2
```

También podemos usar la función `difference_in_days()` para calcular la diferencia en días entre nuestras fechas:

```Gleam
let diferenciaEnDias = Date.difference_in_days(fecha1, fecha2)
```

Recuerda que puedes usar estas funciones y operadores no solo para comparar fechas, sino también para ordenar una lista de fechas o realizar otras operaciones.

## Profundizando

En momentos más avanzados, es posible que necesitemos realizar comparaciones más específicas, como verificar si una fecha cae en un día de la semana particular o en un mes en particular. Para esto, podemos utilizar la función `Date.dateparts()` que nos permite desglosar una fecha en diferentes partes, como el día de la semana o el mes.

Por ejemplo, para verificar si una fecha cae en un sábado, podemos hacer lo siguiente:

```Gleam
let desglose = Date.dateparts(fecha)
if desglose.weekday == 6 {
  // La fecha cae en un sábado
}
```

Además, Gleam también tiene un módulo llamado `Chrono` que nos proporciona funciones adicionales para manejar fechas y tiempos. Puedes explorar más sobre este módulo en la documentación de Gleam.

## Ver También

- [Documentación de Gleam](https://gleam.run/documentation/)
- [Módulo Chrono de Gleam](https://gleam.run/modules/chrono/latest/)
- [Ejemplos de código de comparación de fechas en Gleam](https://github.com/gleam-lang/gleam/tree/main/examples/dates)