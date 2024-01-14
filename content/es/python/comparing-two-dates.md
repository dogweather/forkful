---
title:    "Python: Comparando dos fechas"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

<html>
<img src="https://image.freepik.com/vector-gratis/simbolo-python-concepto-programacion-isometrica_39422-204.jpg" height="200" width="500">


## Por qué

Comparar fechas es una tarea común en la programación, especialmente cuando se trabaja con datos que incluyen una marca de tiempo o cuando se desea realizar una funcionalidad basada en cierta fecha. En este artículo, aprenderás cómo realizar comparaciones de fechas en Python de forma efectiva.

## Cómo hacerlo

En Python, existen varias formas de comparar fechas. La forma más sencilla es utilizando los operadores de comparación, como el igual (==), el diferente (!=), el menor que (<) y el mayor que (>).

Veamos un ejemplo en código de Python:
```
fecha1 = "2021-10-10"
fecha2 = "2021-11-15"
if fecha1 < fecha2:
    print("La fecha 1 es anterior a la fecha 2")
elif fecha1 == fecha2:
    print("Las fechas son iguales")
else:
    print("La fecha 2 es anterior a la fecha 1")
```
En este ejemplo, declaramos dos variables de tipo string que contienen fechas. Luego, utilizamos el operador de comparación "<" para verificar si la fecha1 es anterior a fecha2. Si la condición es verdadera, se imprimirá en pantalla "La fecha 1 es anterior a la fecha 2". Si la condición no se cumple, pasa al siguiente bloque de código y verifica si las fechas son iguales utilizando el operador "==". Si ambas fechas son iguales, se imprime en pantalla "Las fechas son iguales". Y finalmente, si ninguna de las condiciones anteriores se cumple, se imprime en pantalla "La fecha 2 es anterior a la fecha 1".

## Profundizando

Una forma más avanzada de comparar fechas en Python es utilizando el módulo "datetime". Este módulo nos permite trabajar con objetos de tipo fecha y realizar operaciones con ellos. Veamos un ejemplo:
```
from datetime import datetime
fecha1 = datetime(2021, 10, 10)
fecha2 = datetime(2021, 11, 15)
if fecha1 > fecha2:
    print("La fecha 1 es después de la fecha 2")
else:
    print("La fecha 2 es después de la fecha 1")
```
En este ejemplo, utilizamos el módulo "datetime" para crear dos objetos de tipo fecha. Luego, comparamos las fechas utilizando el operador ">" y se imprime en pantalla el resultado según la condición evaluada.

Además de los operadores de comparación, el módulo "datetime" también nos proporciona métodos para realizar operaciones más específicas con fechas, como obtener el día de la semana, el mes o el año de una fecha determinada.

## Ver también

- Documentación oficial de Python sobre el módulo "datetime" [https://docs.python.org/es/3/library/datetime.html]
- Ejemplos de código para comparar fechas en Python [https://www.geeksforgeeks.org/comparing-dates-python/]