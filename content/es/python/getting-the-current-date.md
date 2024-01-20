---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:16:15.345637-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Obtener la fecha actual en Python significa capturar la fecha del sistema en el momento de la ejecución. Los programadores usan esta funcionalidad para registros de tiempo, funciones de caducidad, y cualquier tarea que necesite sincronización con el calendario real.

## Cómo hacerlo:
La forma más fácil y común de obtener la fecha actual en Python es usando el módulo `datetime`. Aquí, un ejemplo sencillo:

```Python
from datetime import date

hoy = date.today()

print(f"La fecha de hoy es: {hoy}")
```

Ejecútalo y verás algo similar a:

```
La fecha de hoy es: 2023-04-01
```

## Inmersión Profunda
La biblioteca estándar de Python ha incluido `datetime` desde la versión 2.3. Antes de `datetime`, los programadores solían lidiar con el módulo `time`, que era menos intuitivo para manejar fechas. Una alternativa moderna es `arrow`, una librería que simplifica muchas operaciones de tiempo y fecha.

Cada objeto de fecha en `datetime` se trata de un objeto inmutable con tres atributos: `year`, `month` y `day`. Python almacena la fecha y hora en UTC internamente, pero con `datetime` puedes fácilmente convertirlas a cualquier zona horaria.

## Ver También
- La documentación oficial de `datetime`: https://docs.python.org/3/library/datetime.html
- Python `time` module documentation for historical context: https://docs.python.org/3/library/time.html
- Arrow: A sensible, human-friendly library for dealing with dates and times: https://arrow.readthedocs.io/en/latest/