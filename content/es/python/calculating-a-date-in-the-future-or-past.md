---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Python: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Calcular una fecha en el pasado o en el futuro es una tarea común para los programadores, ya que a menudo necesitamos realizar operaciones con fechas para nuestras aplicaciones. Por ejemplo, podemos necesitar mostrar la fecha de cumplimiento de un evento o calcular la edad de un usuario.

## Cómo:
Para calcular una fecha en el futuro o en el pasado en Python, podemos usar el módulo datetime. Primero, debemos importar el módulo usando la palabra clave ```import``` y luego podemos usar la función ```date``` para crear un objeto de fecha con la fecha deseada. A continuación, podemos usar el método ```replace``` para cambiar el valor de la fecha y obtener la fecha deseada en el futuro o en el pasado. Aquí hay un ejemplo de cómo calcular la fecha de hoy menos 30 días:

```Python
import datetime

hoy = datetime.date.today()
fecha_pasada = hoy.replace(day=hoy.day-30)
print(fecha_pasada)
```
Output:
```
2020-04-07
```

## Inmersión profunda:
El cálculo de fechas es un problema común en la programación y ha sido una tarea importante durante décadas. Antes de que existieran los lenguajes de programación, la calidad de los calendarios era muy pobre y no había una forma estándar de representar las fechas. Sin embargo, con el advenimiento de los lenguajes de programación y la estandarización de la representación de fechas, el cálculo de fechas se ha vuelto más fácil y preciso.

Existen otras formas de calcular una fecha en el futuro o en el pasado en Python, como usando el módulo ```timedelta```, el cual permite especificar una cantidad de tiempo para agregar o restar a una fecha. También hay módulos de terceros disponibles para cálculos más complejos de fechas.

En cuanto a la implementación, debemos tener en cuenta el formato en el que queremos mostrar la fecha, ya que puede variar según el país o la región. También debemos asegurarnos de manejar adecuadamente los años bisiestos y los diferentes calendarios utilizados en diferentes culturas.

## Ver también:
- Documentación oficial de Python sobre el módulo datetime: https://docs.python.org/es/3/library/datetime.html
- Módulo timedelta de Python:https://docs.python.org/es/3/library/datetime.html#timedelta-objects
- Paquete dateutil de terceros para operaciones avanzadas con fechas: https://dateutil.readthedocs.io/en/stable/