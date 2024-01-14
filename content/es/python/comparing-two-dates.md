---
title:                "Python: Comparando dos fechas"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

# ¿Por qué comparar dos fechas en Python?

Comparar dos fechas en Python puede ser muy útil cuando se trabaja con datos que están asociados con fechas, como registros de ventas o entradas de un diario. También puede ser útil para asegurar que las fechas ingresadas por los usuarios sean válidas en una aplicación.

## Cómo hacerlo

Para comparar dos fechas en Python, se pueden utilizar los operadores de comparación `==` (igual), `!=` (no igual), `<` (menor que), `>` (mayor que), `<=` (menor o igual) y `>=` (mayor o igual). También se pueden utilizar las funciones `datetime.date()` y `datetime.datetime()` para crear objetos de fecha y tiempo.

Por ejemplo, para comparar si dos fechas son iguales, se puede utilizar el operador `==` de la siguiente manera:
````Python
import datetime

fecha1 = datetime.date(2020, 5, 10)
fecha2 = datetime.date(2020, 5, 10)

if fecha1 == fecha2:
    print("Las fechas son iguales")
````

Si se desea comprobar si una fecha es posterior a otra, se puede utilizar el operador `>` de la siguiente manera:
````Python
import datetime

fecha1 = datetime.date(2020, 5, 10)
fecha2 = datetime.date(2020, 5, 5)

if fecha1 > fecha2:
    print("La fecha 1 es posterior a la fecha 2")
````

También se pueden comparar objetos de fecha y tiempo utilizando las funciones `datetime.datetime()` y `datetime.timedelta()`. Esta última permite definir una diferencia de tiempo entre dos fechas. Por ejemplo, si se desea comparar si una fecha es anterior a otra por un periodo de 7 días, se podría hacer de la siguiente manera:
````Python
import datetime

fecha1 = datetime.date(2020, 5, 10)
fecha2 = datetime.date(2020, 5, 3)

if fecha1 < fecha2 + datetime.timedelta(days=7):
    print("La fecha 1 es anterior a la fecha 2 por menos de 7 días")
````

## Profundizando en la comparación de fechas

Es importante tener en cuenta que las fechas y horas en Python son objetos complejos con diferentes propiedades y métodos. Por ejemplo, se pueden obtener componentes individuales de una fecha, como el año, mes y día, utilizando los métodos `year`, `month` y `day` respectivamente. También se pueden realizar operaciones matemáticas con fechas, como sumar o restar días, utilizando el método `timedelta()` mencionado anteriormente.

Es importante tener cuidado al comparar diferentes formatos de fecha y asegurarse de que se estén comparando objetos de fecha y tiempo del mismo tipo.

# Ver También

- [Documentación oficial de Python sobre el módulo datetime](https://docs.python.org/es/3/library/datetime.html)
- [Tutorial de Real Python sobre cómo manejar fechas y horas en Python](https://realpython.com/python-datetime/)
- [Comparación de fechas con Python en GeeksforGeeks](https://www.geeksforgeeks.org/working-with-date-and-time-in-python/)