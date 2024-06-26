---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:18.741903-07:00
description: "C\xF3mo hacerlo: La biblioteca est\xE1ndar de Python proporciona el\
  \ m\xF3dulo `datetime`, que incluye el m\xE9todo `strptime` para este prop\xF3sito.\
  \ El m\xE9todo requiere\u2026"
lastmod: '2024-03-13T22:44:58.622049-06:00'
model: gpt-4-0125-preview
summary: "La biblioteca est\xE1ndar de Python proporciona el m\xF3dulo `datetime`,\
  \ que incluye el m\xE9todo `strptime` para este prop\xF3sito."
title: Analizando una fecha a partir de una cadena de texto
weight: 30
---

## Cómo hacerlo:
La biblioteca estándar de Python proporciona el módulo `datetime`, que incluye el método `strptime` para este propósito. El método requiere dos argumentos: la cadena de fecha y una directiva de formato que especifica el patrón de la cadena de entrada.

```python
from datetime import datetime

# Cadena de ejemplo
date_string = "2023-04-01 14:30:00"
# Analizando cadena a objeto datetime
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

print(parsed_date)
# Salida: 2023-04-01 14:30:00
```

Para un análisis de fechas más matizado, especialmente al tratar con múltiples formatos o localidades, la biblioteca de terceros `dateutil` puede ser extremadamente útil. Proporciona un módulo de análisis que puede interpretar fechas en casi cualquier formato de cadena.

```python
from dateutil import parser

# Cadenas de ejemplo
date_string1 = "April 1, 2023 2:30 PM"
date_string2 = "1st April 2023 14:30"

# Usando el analizador de dateutil
parsed_date1 = parser.parse(date_string1)
parsed_date2 = parser.parse(date_string2)

print(parsed_date1)
# Salida: 2023-04-01 14:30:00
print(parsed_date2)
# Salida: 2023-04-01 14:30:00
```

`dateutil` es hábil en manejar la mayoría de los formatos de fecha sin cadenas de formato explícitas, lo que lo convierte en una opción versátil para aplicaciones que tratan con representaciones de fecha diversas.
