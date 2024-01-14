---
title:    "Python: Obteniendo la fecha actual"
keywords: ["Python"]
---

{{< edit_this_page >}}

## ¿Por qué deberías obtener la fecha actual en Python?

Obtener la fecha actual en Python puede ser útil en muchas situaciones, como registrar la fecha de creación de un archivo, generar reportes con datos actualizados o simplemente para tener una forma fácil de obtener la fecha actual en tu código.

## Cómo obtener la fecha actual en Python

Para obtener la fecha actual en Python, podemos utilizar el módulo `datetime`. Este módulo nos permite trabajar con objetos de fecha y hora de manera sencilla.

```python
import datetime

# Obtenemos la fecha y hora actual
fecha_actual = datetime.datetime.now()

# Imprimimos la fecha actual en formato ISO
print(fecha_actual.isoformat())

# Imprimimos solo la fecha en formato día-mes-año
print(fecha_actual.strftime("%d-%m-%Y"))

# Imprimimos solo la hora en formato de 24 horas
print(fecha_actual.strftime("%H:%M"))
```

**Output:**
```
2020-08-01T10:35:05.123456
01-08-2020
10:35
```

## Profundizando en la obtención de la fecha actual en Python

El método `now()` del módulo `datetime` nos devuelve un objeto `datetime` que contiene la fecha y hora actual. Este objeto tiene diferentes métodos que nos permiten manipular la información, como `isoformat()` para obtener la fecha y hora en formato ISO, o `strftime()` para especificar un formato de fecha y hora personalizado.

También podemos obtener información específica de la fecha y hora actual, como el año, mes, día, hora, minutos y segundos, utilizando los métodos `year`, `month`, `day`, `hour`, `minute` y `second`, respectivamente.

## Ver también

- Documentación oficial de `datetime`: https://docs.python.org/es/3/library/datetime.html
- Tutorial de Python: Fechas y horas en Python: https://www.pythonforbeginners.com/basics/python-datetime-tutorial
- Obtener la fecha actual en Python con `time` y `datetime`: https://www.analyticslane.com/2020/05/26/obtener-la-fecha-hora-actual-en-python-con-time-y-datetime/