---
title:                "Calcular una fecha en el futuro o pasado"
date:                  2024-01-20T17:31:49.356170-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcular una fecha en el futuro o pasado"

category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Calcular una fecha en el futuro o pasado es simplemente sumar o restar días a una fecha dada. Los programadores hacen esto para manejar eventos, suscripciones, recordatorios y todo lo relacionado con fechas y plazos.

## Cómo hacerlo:
```Python
from datetime import datetime, timedelta

# Fecha actual
hoy = datetime.now()

# Calcular dos semanas en el futuro
futuro = hoy + timedelta(weeks=2)
print(f"futuro: {futuro}")

# Calcular 30 días en el pasado
pasado = hoy - timedelta(days=30)
print(f"pasado: {pasado}")
```
Salida de ejemplo:
```
futuro: 2023-04-14 16:45:02.106417
pasado: 2023-03-05 16:45:02.106417
```

## Profundización
Históricamente, la gestión de fechas ha sido compleja debido a consideraciones como zonas horarias y años bisiestos. Python facilita estas tareas con el módulo `datetime`, que ofrece herramientas robustas para la manipulación de fechas y horas.

Alternativas a `datetime` incluyen paquetes de terceros como `dateutil`, que proporciona funciones extendidas, y `pandas`, popular en análisis de datos por su manejo eficiente de series temporales.

En cuanto a implementación, `timedelta` es una clase clave que representa una duración, la diferencia entre dos fechas u horas, y se puede sumar o restar a objetos `datetime` para calcular fechas en el futuro o pasado.

## Ver También

- [Documentación oficial de `datetime`](https://docs.python.org/3/library/datetime.html)
- [PyPI `dateutil`](https://pypi.org/project/python-dateutil/)
- [Tutorial de `pandas` para series temporales](https://pandas.pydata.org/pandas-docs/stable/user_guide/timeseries.html)
