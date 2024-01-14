---
title:    "Elm: Calculando una fecha en el futuro o pasado."
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Por qué

Calcular fechas en el futuro o en el pasado puede ser una tarea útil cuando se están construyendo aplicaciones que requieren de funcionalidades como recordatorios, eventos programados, o simplemente para mostrar la fecha en un formato más fácil de entender para el usuario.

# Cómo hacerlo

En Elm, hay varias maneras de calcular fechas en el futuro o en el pasado. Una manera sencilla es utilizar la función `add` del módulo `Time`. Por ejemplo, si queremos calcular la fecha de hoy más una semana, podemos escribir lo siguiente:

```Elm
add (-1 * 7 * Time.day) (Time.now)
```

Este código toma la fecha actual (utilizando `Time.now`) y le suma un número de días (en este caso, 7) multiplicado por el valor de `Time.day` (que representa un día en milisegundos). El resultado será la fecha de hoy más una semana.

Si queremos calcular la fecha en el pasado, podemos simplemente cambiar el signo del número de días. Por ejemplo:

```Elm
add (3 * Time.day) (Time.now)
```

Este código nos devolverá la fecha de hoy más tres días en el futuro.

Otra opción es utilizar el módulo `Date` para trabajar exclusivamente con fechas. Por ejemplo, si queremos calcular la fecha de hoy más un mes, podemos utilizar la función `addMonth` del módulo `Date`. El código se vería así:

```Elm
Date.fromTime (addMonth 1 (Time.now))
```

Este código toma la fecha actual, la pasa a una fecha de tipo `Date` y luego le suma un mes.

# Profundizando

Existen varias funciones y módulos en Elm que pueden ser de utilidad para calcular fechas en el futuro o en el pasado, como por ejemplo `isBefore` y `isAfter` del módulo `Time`, que nos permiten comparar fechas para ver si una es anterior o posterior a otra. Otra función útil es `toMonthName` del módulo `Date` que nos devuelve el nombre del mes correspondiente a una fecha dada.

Sin embargo, es importante tener en cuenta que trabajar con fechas puede ser complejo debido a la variabilidad de los calendarios, zonas horarias y cambios en las políticas de horario de verano. Por esta razón, es recomendable utilizar librerías o módulos creados específicamente para trabajar con fechas y evitar posibles errores.

# Ver también

- Documentación oficial de Elm sobre fechas y tiempo: https://package.elm-lang.org/packages/elm/time/latest/
- Librería `ianmackenzie/elm-time-extra` que ofrece funciones adicionales para trabajar con fechas y tiempo en Elm: https://package.elm-lang.org/packages/ianmackenzie/elm-time-extra/latest/
- Tutorial de Elm sobre cómo trabajar con fechas y tiempo: https://elmprogramming.com/dates-and-time-in-elm.html