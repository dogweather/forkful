---
title:                "Analizando una fecha de una cadena"
html_title:           "Python: Analizando una fecha de una cadena"
simple_title:         "Analizando una fecha de una cadena"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La conversión de una fecha en formato de texto a un formato de fecha en Python se conoce como "analizar una fecha desde una cadena". Los programadores hacen esto para poder manejar y manipular fechas en sus programas de una manera más eficiente y precisa.

## Cómo:

```Python
# Importar el módulo datetime
import datetime

# Crear una cadena de texto con la fecha
date_string = '12/03/2021'

# Analizar la fecha desde la cadena y guardarla en una variable
date = datetime.datetime.strptime(date_string, '%d/%m/%Y')

# Imprimir la fecha en un formato específico
print(date.strftime('%A, %d de %B del %Y'))
```

**Salida:** Friday, 12 de March del 2021

## Deep Dive:

**Contexto histórico:** La necesidad de analizar fechas desde cadenas ha aumentado con el creciente uso de la informática en diversos campos, como la contabilidad, la programación y la ciencia de datos. Esta función ha estado presente en Python desde la versión 2.4.

**Alternativas:** Aparte de usar el módulo datetime, también se puede utilizar la librería dateutil para un análisis más flexible y automatizado de fechas.

**Detalles de implementación:** La función strptime del módulo datetime acepta dos argumentos: una cadena de texto que contiene la fecha y un formato que especifica cómo se encuentra la fecha en la cadena. Además, se pueden utilizar diferentes códigos de formato para obtener resultados más complejos, como la hora o el día de la semana.

## Ver también:

- [Documentación oficial de Python sobre el módulo datetime](https://docs.python.org/es/3/library/datetime.html)
- [Preguntas frecuentes sobre fechas en Python](https://pythonhosted.org/dateutil/parser.html#dateutil.parser.parse)
- [Artículo sobre formatear fechas en Python](https://www.programiz.com/python-programming/datetime/strptime)