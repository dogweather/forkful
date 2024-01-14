---
title:                "Python: Calculando una fecha en el futuro o en el pasado."
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Hay muchas razones por las cuales uno pueda necesitar calcular una fecha en el futuro o en el pasado, especialmente en el contexto de la programación. Puede ser necesario para realizar tareas de programación, como planificación de eventos o generación de informes. Además, es una habilidad importante para tener cuando se trabaja con fechas y horarios en general.

## Cómo

Para calcular una fecha en el futuro o en el pasado en Python, utilizamos el módulo `datetime`. Primero, importamos el módulo:

```Python
import datetime
```

Luego, podemos crear un objeto `datetime` utilizando la función `datetime()`, especificando los argumentos de año, mes y día:

```Python
fecha = datetime.datetime(2021, 10, 31)
```

Una vez que tenemos nuestro objeto `datetime`, podemos manipularlo para obtener una fecha en el futuro o en el pasado. Por ejemplo, podemos usar el método `timedelta()` para agregar o restar días, semanas, meses o años a nuestra fecha:

```Python
# fecha en el futuro
fecha_futura = fecha + datetime.timedelta(days=7)
print(fecha_futura)

# fecha en el pasado
fecha_pasada = fecha - datetime.timedelta(months=3)
print(fecha_pasada)
```

El resultado se imprimirá en formato `YYYY-MM-DD HH:MM:SS`.

## Profundizando

El módulo `datetime` también nos brinda otras funciones y métodos para trabajar con fechas y horarios. Por ejemplo, podemos obtener el día, mes y año de una fecha específica utilizando los métodos `day`, `month` y `year` respectivamente. También podemos formatear la fecha en un formato personalizado utilizando el método `strftime()`. Puedes encontrar más información sobre estos métodos y otras funciones útiles en la documentación oficial del módulo.

## Ver también

- [Documentación oficial del módulo `datetime` en Python](https://docs.python.org/es/3/library/datetime.html)
- [Tutorial de programación con fechas en Python](https://realpython.com/python-datetime/)
- [Video tutorial sobre cómo trabajar con fechas en Python](https://www.youtube.com/watch?v=eirjjyP2qcQ)