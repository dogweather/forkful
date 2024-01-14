---
title:    "Python: Calculando una fecha en el futuro o pasado."
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez has necesitado saber la fecha en el futuro o en el pasado? ¿Quizás para planificar un viaje o para recordar una fecha importante? En este artículo, aprenderemos cómo calcular fechas en el futuro o en el pasado utilizando Python.

## Cómo hacerlo
Utilizaremos la biblioteca datetime de Python para realizar el cálculo de fechas. Primero, importemos la biblioteca en nuestro código:

```Python
import datetime
```

Para calcular una fecha en el futuro, podemos utilizar el método `timedelta` de la biblioteca datetime. Este método nos permite agregar o restar una cantidad deseada de días a una fecha específica. Por ejemplo, si queremos saber la fecha dentro de dos semanas a partir de hoy, podemos hacer lo siguiente:

```Python
fecha_hoy = datetime.date.today()
print("Hoy es:", fecha_hoy)

fecha_futura = fecha_hoy + datetime.timedelta(days=14)
print("La fecha dentro de dos semanas será:", fecha_futura)
```
El resultado de este código será:

```
Hoy es: 2021-07-15
La fecha dentro de dos semanas será: 2021-07-29
```

Para calcular una fecha en el pasado, podemos utilizar el mismo método `timedelta`, pero en lugar de sumar, restaremos una cantidad de días. Por ejemplo, si queremos saber la fecha de hace un mes a partir de hoy, podemos hacer lo siguiente:

```Python
fecha_pasada = fecha_hoy - datetime.timedelta(days=30)
print("La fecha de hace un mes fue:", fecha_pasada)
```

El resultado de este código será:

```
La fecha de hace un mes fue: 2021-06-15
```

## Profundizando
Además de los días, también podemos utilizar otros parámetros en el método `timedelta` como semanas, horas, minutos, segundos, entre otros. Esto nos da una gran flexibilidad para calcular fechas en el futuro o en el pasado de acuerdo a nuestras necesidades.

También podemos utilizar el método `replace` para cambiar una parte específica de una fecha, como el año, el mes o el día. Por ejemplo, si queremos saber la fecha de nuestro cumpleaños en el año 2022, podemos hacer lo siguiente:

```Python
fecha_cumple = fecha_hoy.replace(year=2022)
print("Mi cumpleaños será:", fecha_cumple)
```

El resultado de este código será:

```
Mi cumpleaños será: 2022-07-15
```

## Ver también
- [Documentación oficial de la biblioteca datetime de Python](https://docs.python.org/es/3/library/datetime.html)
- [Tutorial de Real Python sobre cálculos de fechas en Python](https://realpython.com/python-datetime/)
- [Python para principiantes: manejo de fechas y horas](https://www.learnpython.org/es/Dates_and_Times)