---
title:                "Python: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué comparar dos fechas

Comparar dos fechas es una habilidad importante en la programación ya que nos permite realizar diversas tareas, como verificar si una fecha es anterior o posterior a otra, o calcular la diferencia entre dos fechas. Además, es una técnica esencial para trabajar con bases de datos, ya que nos permite filtrar y ordenar información por fechas.

## Cómo comparar dos fechas en Python

Para comparar dos fechas en Python, primero debemos asegurarnos de tener ambas fechas en un formato compatible. Por ejemplo, podemos usar la librería datetime para convertir fechas en formato de texto a un objeto de fecha:

```Python
from datetime import datetime

fecha1 = datetime.strptime("10/05/2020", "%d/%m/%Y")
fecha2 = datetime.strptime("23/12/2020", "%d/%m/%Y")

if fecha1 < fecha2:
    print("La fecha 1 es anterior a la fecha 2")
elif fecha1 > fecha2:
    print("La fecha 1 es posterior a la fecha 2")
else:
    print("Ambas fechas son iguales")
```

Este ejemplo utiliza la función strptime para convertir una cadena de texto en fecha, especificando el formato de la fecha en el segundo parámetro. Luego, se puede usar los operadores de comparación para determinar si una fecha es anterior, posterior o igual a otra.

## Profundizando en la comparación de fechas

Cuando comparamos fechas, debemos tener en cuenta varios factores. Uno de ellos es el formato de la fecha, ya que difiere de un país a otro (por ejemplo, el día puede ir antes del mes o viceversa). También debemos tener en cuenta que una fecha puede contener horas, minutos y segundos, lo que puede afectar a la comparación.

En Python, podemos utilizar métodos de la clase datetime para manipular y comparar fechas con más flexibilidad. Por ejemplo, podemos usar el método difference para calcular la diferencia en días entre dos fechas:

```Python
diferencia = fecha2 - fecha1
print("La diferencia en días es:", diferencia.days)
```

También podemos usar el método strftime para cambiar el formato de la fecha y comparar solo parte de ella. Por ejemplo, si queremos comparar solo los años de dos fechas, podemos hacer lo siguiente:

```Python
año1 = fecha1.strftime("%Y")
año2 = fecha2.strftime("%Y")

if año1 < año2:
    print("El año de la fecha 1 es anterior al de la fecha 2")
elif año1 > año2:
    print("El año de la fecha 1 es posterior al de la fecha 2")
else:
    print("Ambos años son iguales")
```

## Ver también

- [Documentación de datetime en Python](https://docs.python.org/es/3.9/library/datetime.html)
- [Tutorial de Python para principiantes](https://www.python.org.ar/aprendiendo-python/)