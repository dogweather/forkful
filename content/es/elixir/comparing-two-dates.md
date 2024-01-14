---
title:    "Elixir: Comparando dos fechas"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en Elixir?

Comparar dos fechas es una tarea común en la programación. Esta acción permite determinar si una fecha es posterior o anterior a otra, o si ambas son iguales. En Elixir, esta tarea se puede realizar de manera sencilla y eficiente gracias a las funciones y operadores disponibles en el lenguaje. En este artículo, aprenderemos cómo comparar dos fechas en Elixir de manera fácil y rápida.

## Cómo hacerlo

En Elixir, hay varias formas de comparar dos fechas. Una de ellas es utilizando el operador de comparación `==`, que compara si ambas fechas son iguales. Por ejemplo:

```Elixir
date1 = ~D[2021-07-10]
date2 = ~D[2021-07-10]
date1 == date2 # Output: true
```

También se puede utilizar el operador `>`, que determina si una fecha es posterior a otra. Por ejemplo:

```Elixir
date1 = ~D[2021-07-11]
date2 = ~D[2021-07-10]
date1 > date2 # Output: true
```

Si se desea comparar si una fecha es anterior a otra, se puede utilizar el operador `<`. Por ejemplo:

```Elixir
date1 = ~D[2021-07-09]
date2 = ~D[2021-07-10]
date1 < date2 # Output: true
```

Otra forma de comparar dos fechas es utilizando la función `Date.compare/2`, que devuelve un número entero para indicar si la primera fecha es anterior, igual o posterior a la segunda fecha. Si el resultado es -1, significa que la primera fecha es anterior, si es 0 significa que ambas son iguales, y si es 1 significa que la primera fecha es posterior. Por ejemplo:

```Elixir
date1 = ~D[2021-07-10]
date2 = ~D[2021-07-11]
Date.compare(date1, date2) # Output: -1
```

Además, también se puede utilizar la función `Date.before?/2` y `Date.after?/2` para determinar si una fecha es anterior o posterior a otra, respectivamente. Estas funciones devuelven un booleano. Por ejemplo:

```Elixir
date1 = ~D[2021-07-09]
date2 = ~D[2021-07-10]
Date.before?(date1, date2) # Output: true
Date.after?(date1, date2) # Output: false
```

## Profundizando

Es importante tener en cuenta que en Elixir, las fechas se representan como estructuras de datos, lo que permite comparar no solo la fecha sino también la hora y los segundos, si se desean. Por ejemplo:

```Elixir
datetime1 = ~N[2021-07-10 12:00:00]
datetime2 = ~N[2021-07-10 14:00:00]
datetime1 > datetime2 # Output: false
```

Además, Elixir también cuenta con módulos como `DateTime`, `NaiveDateTime` y `Time` que brindan funciones y operadores para trabajar con fechas y horas. Se recomienda explorar estos módulos para tener un mayor control y flexibilidad al comparar fechas en Elixir.

## Ver también

- Documentación oficial de Elixir sobre fechas y tiempo: https://elixir-lang.org/getting-started/datetime.html
- Artículo sobre cómo trabajar con fechas y horas en Elixir: https://www.pluralsight.com/guides/dates-and-times-in-elixir
- Ejemplos de código para comparar fechas en Elixir: https://gist.github.com/phortela/c4575ab81dbfa8450b844f0fac61edeb