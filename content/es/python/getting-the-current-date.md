---
title:                "Python: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Obtener la fecha actual puede ser útil para muchas tareas de programación, como crear registros de tiempo o programar tareas para una fecha específica.

## Cómo hacerlo

Para obtener la fecha actual en Python, primero debemos importar el módulo `datetime`.

```Python
import datetime
```

Luego, podemos usar la función `datetime.now()` para obtener la fecha y la hora actuales en un objeto `datetime`.

```Python
ahora = datetime.now()
print(ahora)
```

Esto imprimirá la fecha y hora actuales en el siguiente formato: `YYYY-MM-DD HH:MM:SS.mmmmmm`.

Si solo queremos obtener la fecha actual en el formato `DD-MM-YYYY`, podemos usar la función `date()`.

```Python
hoy = datetime.date.today()
print(hoy)
```

¡Y listo! Ahora tenemos la fecha actual almacenada en la variable `hoy` para usarla en nuestro código.

## Profundizando

El módulo `datetime` también tiene otras funciones útiles para trabajar con fechas y horas. Aquí hay algunos ejemplos:

- `datetime.combine(date, time)`: combina un objeto `date` y un objeto `time` en un solo objeto `datetime`.
- `datetime.strptime(date, format)`: convierte una cadena de fecha en un objeto `datetime`, utilizando el formato especificado.
- `datetime.replace(year, month, day)`: reemplaza la fecha actual en un objeto `datetime` con una nueva fecha especificada.

Para obtener más información sobre cómo trabajar con fechas y horas en Python, se recomienda consultar la documentación oficial del módulo `datetime`.

## Ver también

- [`datetime` module documentation](https://docs.python.org/es/3/library/datetime.html)
- [`time` module documentation](https://docs.python.org/es/3/library/time.html)
- [Python's Datetime Library by Real Python](https://realpython.com/python-datetime/#gathering-user-input-for-dates-and-times)