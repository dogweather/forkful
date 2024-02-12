---
title:                "Analizando una fecha a partir de una cadena de texto"
aliases:
- /es/python/parsing-a-date-from-a-string/
date:                  2024-02-03T19:15:18.741903-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analizando una fecha a partir de una cadena de texto"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?
Analizar una fecha de una cadena implica convertir información textual de fecha y hora en un objeto datetime o un formato estructurado equivalente. Esto se realiza comúnmente para habilitar operaciones aritméticas, comparaciones y formateos de fechas de manera independiente del idioma y la región. Los programadores lo hacen para manejar y manipular eficientemente datos temporales extraídos de registros, entradas de usuarios o fuentes externas.

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
