---
title:                "Python: Obteniendo la fecha actual"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

# ¿Por qué obtener la fecha actual con Python? 

La fecha actual es una información valiosa que se puede utilizar en una variedad de proyectos de programación. Desde crear aplicaciones de calendario hasta rastrear eventos en una base de datos, tener acceso a la fecha actual es esencial para muchos programas. En esta publicación del blog, exploraremos cómo obtener la fecha actual utilizando Python y cómo puede ser útil para tu trabajo de desarrollo.

## Cómo hacerlo

Python proporciona una forma sencilla y eficiente de obtener la fecha actual a través del módulo `datetime`. Primero, debes importar este módulo en tu código:

```python
import datetime
```

Luego, puedes utilizar la función `datetime.now()` para obtener la fecha actual en el formato de objeto de fecha y hora de Python:

```python
current_date = datetime.now()
```

Si deseas solo la fecha actual sin la hora, puedes utilizar la función `today()` en lugar de `now()`:

```python
current_date = datetime.today()
```

También puedes especificar la zona horaria deseada utilizando el argumento `tz` en `datetime.now()` o `today()`:

```python
from pytz import timezone
central_time = timezone('US/Central')
current_date = datetime.now(tz=central_time)
```

## Un análisis más profundo

El módulo `datetime` también te permite realizar diversas operaciones con la fecha actual, como formatearla en una cadena legible o extraer componentes específicos, como el día, el mes o el año. Aquí hay algunos ejemplos de cómo puedes hacer esto:

```python
current_date_str = current_date.strftime("%d/%m/%Y") # formato dd/mm/yyyy
day = current_date.day # día actual
month = current_date.month # mes actual
year = current_date.year # año actual
```

Además, puedes usar la función `isoweekday()` para obtener el día de la semana en un formato numérico, siendo 1 para el lunes y 7 para el domingo:

```python
weekday = current_date.isoweekday() # día de la semana actual (número)
```

También es posible realizar operaciones aritméticas con fechas y horas utilizando el módulo `timedelta`. Por ejemplo, para obtener la fecha de hoy hace 30 días, puedes hacer lo siguiente:

```python
from datetime import timedelta
thirty_days_ago = current_date - timedelta(days=30)
```

## Ver también

Si deseas obtener más información sobre cómo trabajar con fechas en Python, te recomendamos consultar la documentación oficial de `datetime` (https://docs.python.org/3/library/datetime.html) y `pytz` (https://pypi.org/project/pytz/).

¡Esperamos que esta publicación del blog te haya resultado útil para comprender cómo obtener la fecha actual con Python! Si tienes alguna pregunta o comentario, no dudes en compartirlos en la sección de comentarios a continuación.

## Véase también

- Documentación de datetime: https://docs.python.org/3/library/datetime.html
- Documentación de pytz: https://pypi.org/project/pytz/