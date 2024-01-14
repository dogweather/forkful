---
title:                "Python: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Porqué

Convertir una fecha en una cadena es un proceso común en la programación, ya sea para almacenarla en una base de datos, mostrarla en un formato específico o realizar operaciones matemáticas con ella. En este artículo, exploraremos cómo realizar esta conversión en Python, uno de los lenguajes de programación más populares en la actualidad.

## Cómo hacerlo

Para convertir una fecha en una cadena en Python, podemos utilizar el método `strftime()` de la clase `datetime`. Este método nos permite especificar un formato de fecha y hora en el que queremos que se muestre la fecha. Por ejemplo, si queremos mostrar la fecha en formato "dd/mm/aaaa", podemos escribir el siguiente código:

```python
from datetime import datetime

fecha = datetime(2021, 9, 1)
fecha_string = fecha.strftime('%d/%m/%Y')

print(fecha_string)
```

El resultado de este código sería "01/09/2021", ya que hemos especificado el formato deseado en la función `strftime()`. Podemos personalizar el formato de la fecha y hora utilizando distintas combinaciones de letras, como %d para el día, %m para el mes y %Y para el año. También podemos agregar otros detalles como la hora, los minutos y los segundos.

Es importante tener en cuenta que la función `strftime()` solo funciona en objetos `datetime`, por lo que debemos asegurarnos de crear uno antes de aplicar la conversión.

## Profundizando

Además de la función `strftime()`, también podemos usar la función `format()` para convertir una fecha en una cadena en Python. Esta función nos permite especificar el formato de la fecha y hora de forma similar a `strftime()`, pero en lugar de aplicarlo a un objeto `datetime`, lo aplicamos a una cadena directamente. Por ejemplo:

```python
fecha = '2021/9/1'
fecha_string = fecha.format('%d/%m/%Y')

print(fecha_string)
```

En este caso, hemos aplicado la función `format()` a una cadena, en lugar de a un objeto `datetime`. Esto puede ser útil si ya tenemos una cadena con una fecha y queremos aplicarle un formato específico.

También podemos convertir una fecha en una cadena utilizando la biblioteca `arrow`, que nos ofrece métodos más sencillos y flexibles para manejar fechas y horas en Python. Por ejemplo:

```python
import arrow

fecha = arrow.get(2021, 9, 1)
fecha_string = fecha.format('DD/MM/YYYY')

print(fecha_string)
```

Acá, hemos utilizado el método `get()` de la biblioteca `arrow` para crear un objeto de fecha y hora directamente. Luego, usamos el método `format()` para especificar el formato deseado.

## Ver también

- [Documentación oficial de Python en español](https://docs.python.org/es/3/library/datetime.html)
- [Tutorial de fechas y horas en Python](https://www.realpython.com/python-datetime/)
- [Documentación oficial de la biblioteca arrow](https://arrow.readthedocs.io/en/latest/index.html)