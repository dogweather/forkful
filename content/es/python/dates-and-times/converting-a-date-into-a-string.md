---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "C\xF3mo hacerlo: Python facilita la conversi\xF3n de fechas a cadenas.\
  \ Utiliza el m\xE9todo [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-\u2026"
lastmod: '2024-04-04T02:04:02.822404-06:00'
model: gpt-4-0125-preview
summary: "Python facilita la conversi\xF3n de fechas a cadenas."
title: Convirtiendo una fecha en una cadena de texto
weight: 28
---

## Cómo hacerlo:
Python facilita la conversión de fechas a cadenas. Utiliza el método [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) disponible en los objetos de [fecha](https://docs.python.org/3/library/datetime.html#date-objects). Así es cómo:

```Python
from datetime import datetime

# Obtener la fecha y hora actual
now = datetime.now()

# Convertirlo a una cadena en el formato: Mes día, Año
date_string = now.strftime("%B %d, %Y")
print(date_string)  # Salida: March 29, 2023 (o fecha actual)

# Formato: AAAA-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # Salida: 2023-03-29 (o fecha actual)
```


### Cómo lo hago

Así es como obtengo una fecha en formato [ISO 8601](https://www.w3.org/QA/Tips/iso-date) con información de zona horaria:

```python
def datestamp() -> str:
    """ 
    La fecha y hora actual con zona horaria en formato ISO.
    """
    return datetime.now().astimezone().isoformat()
```

#### Ejemplo de salida:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## Profundizando
Históricamente, la conversión de fechas a cadenas ha sido un pilar en la programación debido a la necesidad de representar fechas en un formato legible por humanos.

Alternativas a `strftime` incluyen el uso del método `isoformat` para el formato ISO 8601, o bibliotecas de terceros como `arrow` y `dateutil` que ofrecen opciones de análisis y formato más flexibles.

En cuanto a implementación, `strftime` significa "formato de tiempo de cadena" y tiene raíces en la programación en C. `strftime` de Python interpreta códigos de formato como `%Y` para el año y `%m` para el mes, permitiendo una personalización casi infinita.

## Ver También
Para profundizar en las funciones de fecha y hora de Python:
- Documentación oficial de `datetime` de Python: https://docs.python.org/3/library/datetime.html
- Para aquellos interesados en una lista completa de las directivas de `strftime`: https://strftime.org/
- Para explorar bibliotecas de fecha/hora de terceros:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
