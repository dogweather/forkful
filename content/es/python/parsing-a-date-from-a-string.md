---
title:                "Analizando una fecha desde una cadena de texto"
html_title:           "PHP: Analizando una fecha desde una cadena de texto"
simple_title:         "Analizando una fecha desde una cadena de texto"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Interpretar una fecha desde una cadena de texto (parsing a date from a string) es convertir una fecha escrita como texto en un objeto de fecha que Python puede manipular. Como programadores, hacemos esto para facilitar el trabajo con fechas y hora en nuestros programas.

## Cómo Hacerlo:

Aquí tenemos un ejemplo sobre cómo podemos transformar una cadena de texto a una fecha utilizando la librería datetime de Python.

```Python
from datetime import datetime

fecha_en_texto = "15/03/2020"
fecha_objeto = datetime.strptime(fecha_en_texto, "%d/%m/%Y")

print(fecha_objeto)
```

La salida será:

```Python
2020-03-15 00:00:00
```

## Un Paso Más Allá:

1. **Contexto Histórico:** En las primeras versiones de Python, gestionar fechas y hora era mucho más complicado. No fue hasta Python 1.5.2 en 1999 cuando se añadió la librería `datetime`.

2. **Alternativas:** Otra librería popular para trabajar con fechas y horas en Python es `dateutil`. Es más flexible en los formatos de fechas que puede interpretar.

3. **Detalles de Implementación:** Usando `strptime`, convertimos la cadena de texto en un objeto de fecha. El segundo argumento detalla el formato de la fecha en la cadena de texto.

```Python
datetime.strptime("15/03/2020", "%d/%m/%Y")
```
Aquí, "%d/%m/%Y" indica que la fecha en la cadena está en un formato de día/mes/año.

## Ver También:

1. Documentación oficial de `datetime` en Python: https://docs.python.org/3/library/datetime.html 
2. Documentación oficial de `dateutil.parser`: https://dateutil.readthedocs.io/en/stable/parser.html
3. Stack Overflow - convert string to datetime: https://stackoverflow.com/questions/466345/converting-string-into-datetime