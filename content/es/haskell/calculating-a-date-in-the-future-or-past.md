---
title:    "Haskell: Calculando una fecha en el futuro o pasado."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Por qué

Calcular fechas en el futuro o en el pasado es una tarea muy común en la programación. Puede ser útil para planificar eventos o realizar tareas de manera automática. En este artículo hablaremos sobre cómo calcular fechas en Haskell y cómo podemos hacerlo de manera eficiente.

## Cómo hacerlo

Para calcular una fecha en el futuro o pasado, primero necesitamos definir una función que tome como argumentos una fecha inicial y una cantidad de días a sumar o restar. Podemos utilizar la biblioteca `Data.Time` para manejar fechas en Haskell.

Primero, importemos la biblioteca en nuestro código:

```Haskell
import Data.Time
```

Luego, definamos nuestra función de cálculo de fechas:

```Haskell
calculateDate :: Day -> Integer -> Day
calculateDate initialDate days = addDays days initialDate
```

En esta función, utilizamos la función `addDays` de la biblioteca `Data.Time` para sumar o restar la cantidad de días indicada a la fecha inicial.

Veamos un ejemplo de cómo utilizar esta función:

```Haskell
calculateDate (fromGregorian 2020 5 1) 30
```

En este caso, estamos calculando la fecha 30 días después del 1 de mayo de 2020. El resultado será `2020-05-31`, ya que el año y mes se mantienen iguales y solo se agregan 30 días a la fecha.

También podemos realizar cálculos con fechas anteriores o fechas futuras lejanas, ya que la función `addDays` maneja fechas en el formato `Day` de la biblioteca `Data.Time`.

## En profundidad

Si queremos obtener el día de la semana de una fecha determinada, podemos utilizar la función `toWeekDay` de la biblioteca `Data.Time`.

Por ejemplo, si queremos calcular la fecha 21 días antes del 15 de julio de 2020 y obtener el día de la semana, podemos hacerlo de la siguiente manera:

```Haskell
toWeekDay $ calculateDate (fromGregorian 2020 7 15) (-21)
```

El resultado será `Tuesday`, ya que el 24 de junio de 2020 fue un martes.

También es importante tener en cuenta que las funciones de la biblioteca `Data.Time` trabajan con fechas según el calendario gregoriano, por lo que al utilizar fechas de otros calendarios puede haber discrepancias en los resultados.

# Ver también

- [Funciones de fecha en Haskell](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Documentación de la biblioteca Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)