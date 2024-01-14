---
title:                "Elm: Comparando dos fechas"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Por qué comparar dos fechas en Elm

Al programar en Elm, puede ser útil comparar dos fechas para determinar cuál es más reciente, por ejemplo, o para filtrar datos basados en fechas. En esta entrada del blog, aprenderemos cómo comparar dos fechas en Elm y profundizaremos un poco en el proceso. ¡Vamos a empezar!

## Cómo hacerlo

Para comparar dos fechas en Elm, primero debemos asegurarnos de que las fechas estén en un formato adecuado. Elm utiliza el módulo `Time` para manejar fechas y horas, por lo que debemos importar este módulo al principio de nuestro archivo:

```Elm
import Time
```

Una vez importado el módulo `Time`, podemos utilizar la función `Time.fromString` para convertir una cadena de texto en una fecha. Por ejemplo, si tenemos la cadena "2020-01-01" como fecha, podemos convertirla de la siguiente manera:

```Elm
fechaEjemplo = Time.fromString "2020-01-01"
```

Ahora que tenemos nuestras dos fechas en un formato adecuado, podemos utilizar las funciones de comparación `Date.comparable` y `Date.compare` para determinar cuál de las dos fechas es más reciente. Por ejemplo, si queremos comparar las fechas "2020-01-01" y "2020-05-01", podemos hacerlo de la siguiente manera:

```Elm
fecha1 = Time.fromString "2020-01-01"
fecha2 = Time.fromString "2020-05-01"
comparacion = Date.compare (Date.comparable fecha1) (Date.comparable fecha2)
```

La variable `comparacion` ahora tendrá un valor que nos indicará si la fecha1 es anterior, igual o posterior a la fecha2. Podemos utilizar una expresión `case` para manejar cada uno de estos casos.

## Profundizando

Al comparar dos fechas en Elm, es importante tener en cuenta que también podemos utilizar la función `Time.now` para obtener la fecha y hora actuales. Esto puede ser útil para comparar una fecha con la fecha actual, por ejemplo.

También podemos utilizar la función `Time.fromPosix` para crear una fecha a partir de un valor de tiempo POSIX, lo que nos permite trabajar con fechas en una escala más precisa.

## Ver también

- [Documentación oficial de Elm sobre el módulo Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [Artículo sobre comparar fechas en Elm](https://12devsofxmas.co.uk/2013/10/learning-elm-from-a-ruby-developers-perspective-part-3/)
- [Ejemplo de código en GitHub de comparación de fechas en Elm](https://github.com/FidelisClayton/elm-date-comparison-example)