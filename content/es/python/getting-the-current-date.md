---
title:                "Obteniendo la fecha actual"
html_title:           "Python: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué obtener la fecha actual?

Obtener la fecha actual es una tarea común en la programación, especialmente cuando se trata de aplicaciones que requieren información en tiempo real. Ya sea para mostrar la fecha en un sitio web o para realizar cálculos basados en la fecha, obtener la fecha actual es una habilidad importante en cualquier lenguaje de programación, incluyendo Python.

## Cómo obtener la fecha actual

Para obtener la fecha actual en Python, podemos utilizar el módulo incorporado "datetime". Este módulo nos proporciona varias funciones y objetos que nos permiten trabajar con fechas y horas.

Para empezar, podemos importar el módulo en nuestro código de la siguiente manera:

```Python
import datetime
```

Una vez que hayamos importado el módulo, podemos usar la función "datetime.now()" para obtener la fecha y hora actuales. Esta función devuelve un objeto de tipo "datetime", que podemos manipular para obtener información específica de la fecha y hora.

```Python
fecha_actual = datetime.now()
print(fecha_actual)
# Ejemplo de salida: 2021-06-14 12:30:00.430852
```

Además, podemos especificar un formato personalizado para mostrar la fecha utilizando el método "strftime()". Por ejemplo, si queremos mostrar la fecha en formato día / mes / año, podemos hacerlo de la siguiente manera:

```Python
fecha_actual = datetime.now()
fecha_personalizada = fecha_actual.strftime("%d/%m/%Y")
print(fecha_personalizada)
# Ejemplo de salida: 14/06/2021
```

También podemos obtener información específica de la fecha y hora utilizando atributos del objeto "datetime", como "year", "month", "day", "hour", "minute", "second", etc. Por ejemplo:

```Python
fecha_actual = datetime.now()
dia = fecha_actual.day
mes = fecha_actual.month
anio = fecha_actual.year
print("Hoy es el " + str(dia) + " de " + str(mes) + " de " + str(anio))
# Ejemplo de salida: Hoy es el 14 de 6 de 2021
```

## Profundizando en la obtención de la fecha actual

El módulo "datetime" también nos permite trabajar con fechas y horas específicas, en lugar de solo la fecha y hora actual. Podemos crear objetos "datetime" utilizando la función "datetime()", pasando como argumentos los valores del año, mes, día, hora, minuto y segundo. También podemos especificar una zona horaria para el objeto si lo necesitamos.

Además, el módulo "datetime" nos proporciona métodos para realizar cálculos con fechas, como sumar o restar días, meses o años a una fecha determinada, encontrar la diferencia entre dos fechas, comprobar si una fecha es anterior o posterior a otra, entre otros.

Si quieres saber más sobre el módulo "datetime", puedes consultar la documentación oficial de Python o buscar tutoriales en línea para obtener más información y ejemplos prácticos sobre su uso.

## Ver también

- Documentación oficial de Python para el módulo "datetime": https://docs.python.org/es/3/library/datetime.html
- Tutorial de Real Python sobre el trabajo con fechas y horas en Python: https://realpython.com/python-datetime/
- Tutorial de W3Schools sobre el módulo "datetime" en Python: https://www.w3schools.com/python/python_datetime.asp