---
title:                "Calcular una fecha en el futuro o pasado"
html_title:           "Python: Calcular una fecha en el futuro o pasado"
simple_title:         "Calcular una fecha en el futuro o pasado"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# ¿Qué y Por Qué?

Calcular una fecha en el futuro o el pasado es simplemente agregar o quitar una cantidad de tiempo (generalmente días) a una fecha existente. Los programadores hacen esto para realizar programaciones, análisis de datos o para decidir cuándo debe ejecutarse cierto código.

# Cómo Hacerlo
A continuación, te muestro cómo calcular una fecha futura usando la biblioteca `datetime` en Python:

```Python
from datetime import datetime, timedelta

hoy = datetime.now()
print('Hoy:', hoy)

tres_dias_despues = hoy + timedelta(days=3)
print('Tres días después:', tres_dias_despues)
```

Y aquí está la salida de ejemplo:

```Python
Hoy: 2022-05-07 09:42:14.857688
Tres días después: 2022-05-10 09:42:14.857688
```

Y este es un ejemplo de cómo calcular una fecha en el pasado:

```Python
tres_dias_antes = hoy - timedelta(days=3)
print('Tres días antes:', tres_dias_antes)
```

# Buceo Profundo

**Contexto histórico**: La capacidad de calcular fechas en el futuro o el pasado se incorporó a Python en la primera versión de la biblioteca `datetime` en 2003.

**Alternativas**: Aparte de `datetime`, también puedes usar bibliotecas como `delorean`, `arrow`, `pendulum` y `Maya` que pueden realizar fácilmente cálculos de fechas.

**Detalles de implementación**: `timedelta` es una clase que permite operaciones matemáticas con objetos `datetime`. Acepta parámetros como días, segundos, microsegundos, milisegundos, minutos, horas, semanas, y realiza cálculos en función de esos valores.

# Ver También

- Documentación oficial de Python `datetime`: https://docs.python.org/3/library/datetime.html
- Guía de calculo de tiempo y fechas en Python: https://realpython.com/python-datetime/
- Librerías alternativas para manejo de fechas y tiempo: 
  - `delorean`: https://delorean.readthedocs.io/
  - `arrow`: https://arrow.readthedocs.io/
  - `pendulum`: https://pendulum.eustace.io/
  -  `maya`: https://github.com/kennethreitz/maya.