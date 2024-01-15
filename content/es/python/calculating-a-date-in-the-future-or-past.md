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

## Por qué
A veces es necesario calcular una fecha en el futuro o en el pasado para poder planificar eventos o verificar datos históricos. Con la ayuda de la programación en Python, este proceso puede ser automatizado y simplificado.

## Cómo
Para calcular una fecha en el futuro o en el pasado en Python, se utiliza el módulo "datetime". Primero, se importa el módulo utilizando la siguiente línea de código:

```Python
import datetime
```

Luego, se crea un objeto de fecha utilizando la función "datetime" y se le asignan los argumentos necesarios para especificar la fecha deseada. Por ejemplo, para calcular la fecha dentro de 30 días:

```Python
fecha = datetime.datetime(2021, 11, 14) + datetime.timedelta(days=30)
```

El resultado se almacenará en la variable "fecha" en formato de objeto de fecha de Python. Para mostrar la fecha en un formato legible, se puede utilizar la función "strftime":

```Python
fecha_final = fecha.strftime("%d/%m/%Y")
print(fecha_final)
```

La salida en este caso sería "14/12/2021".

## Deep Dive
El módulo "datetime" también ofrece funciones para obtener detalles específicos de una fecha, como el día de la semana o el mes. Además, se puede especificar una hora junto con la fecha para obtener un objeto de fecha y hora completo.

Es importante tener en cuenta que el módulo "datetime" utiliza el calendario gregoriano por defecto. Sin embargo, si se desea utilizar otro calendario, como el judío o el islámico, se pueden utilizar otros módulos de Python específicos para ese calendario.

## Ver También
- Documentación oficial del módulo "datetime" en Python: https://docs.python.org/es/3/library/datetime.html
- Tutorial sobre cómo utilizar el módulo "datetime" en Python: https://realpython.com/python-datetime/
- Explicación detallada sobre los diferentes calendarios en Python: https://www.programiz.com/python-programming/datetime/calendar