---
title:    "Python: Comparando dos fechas"
keywords: ["Python"]
---

{{< edit_this_page >}}

¿Por qué comparar dos fechas en Python?

Las comparaciones de fechas son una parte importante de la programación en Python, ya que nos permiten realizar acciones basadas en la relación entre dos fechas. Esto puede ser útil para determinar si una fecha es anterior o posterior a otra, o para calcular la diferencia entre dos fechas. En este artículo, exploraremos cómo comparar dos fechas en Python y cómo utilizar esta habilidad en tu propio código.

## Cómo hacerlo

Para comparar dos fechas en Python, primero necesitamos importar el módulo `datetime` que nos permite trabajar con fechas y tiempos. Luego, podemos utilizar el método `date` para crear dos objetos de fecha que queremos comparar. Por ejemplo:

```Python
import datetime

# Crear fecha 1
date1 = datetime.date(2021, 6, 1)

# Crear fecha 2
date2 = datetime.date(2021, 6, 15)
```

Una vez que tenemos nuestras dos fechas, podemos utilizar los operadores de comparación `==` (igual), `>` (mayor que), `<` (menor que), `>=` (mayor o igual) y `<=` (menor o igual) para compararlas. Por ejemplo:

```Python
# Comprobar si date1 es igual a date2
print(date1 == date2)  # Output: False

# Comprobar si date1 es mayor que date2
print(date1 > date2)  # Output: False

# Comprobar si date1 es menor que date2
print(date1 < date2)  # Output: True
```

También podemos utilizar el método `timedelta` para calcular la diferencia entre dos fechas en días, semanas o meses. Por ejemplo, para calcular la diferencia de días entre nuestras dos fechas:

```Python
# Calcular la diferencia de días entre date1 y date2
difference = date2 - date1

# Imprimir resultado
print(difference.days)  # Output: 14
```

## Profundizando

Si quieres utilizar comparaciones de fechas más avanzadas en tu código, puedes explorar el módulo `dateutil` de Python. Este módulo nos permite trabajar con fechas y tiempos de una manera más flexible y precisa. Por ejemplo, permite comparar fechas con diferentes formatos o incluso fechas con zonas horarias.

También puedes explorar el módulo `calendar` de Python, que te permite trabajar con calendarios y realizar operaciones como obtener el día de la semana en una fecha específica o cambiar el formato de fechas.

## Ver también

- [Documentación oficial de Python sobre comparar objetos de fecha](https://docs.python.org/3/library/datetime.html#datetime.date)
- [Tutorial sobre el módulo `dateutil`](https://dateutil.readthedocs.io/en/stable/)
- [Tutorial sobre el módulo `calendar`](https://www.programiz.com/python-programming/datetime/calendar)