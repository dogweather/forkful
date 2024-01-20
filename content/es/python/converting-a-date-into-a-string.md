---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
La conversión de una fecha a una cadena (string) en Python implica cambiar un objeto de fecha a su representación de texto*. ¿Por qué lo hacemos? Para hacer la salida más legible para los usuarios y para almacenar la fecha en una base de datos o archivo de texto.

## Cómo hacerlo
La biblioteca datetime en Python nos proporciona el método strftime para hacer este trabajo. Aquí te dejo un ejemplo:
```Python
from datetime import datetime

# Creando una fecha
fecha = datetime(2021, 9, 21)

# Convirtiéndola a cadena
fecha_cadena = fecha.strftime("%d-%m-%Y")

print(fecha_cadena)  # Imprime: 21-09-2021
```

## Profundizando
La función strftime viene del lenguaje C y fue incorporada en Python para facilitar el trabajo con el tiempo. Aunque este método es muy útil, existen alternativas como date.isoformat() en Python, que devuelve una cadena representando la fecha en formato ISO 8601: YYYY-MM-DD.

En cuanto a la implementación, la función strftime toma dos argumentos: el objeto de fecha y tiempo (en nuestro caso, 'fecha') y una cadena de formato que describe cómo la cadena de salida debe ser. Los formatos comunes incluyen:

- %Y: Representa el año completo.
- %m: Representa el mes.
- %d: Representa el día.

## Más información
Para una comprensión más profunda de cómo trabajar con fechas en Python, recomiendo los siguientes enlaces:

1. Documentación de Python sobre el módulo datetime: https://docs.python.org/3/library/datetime.html
2. Un tutorial útil sobre cómo convertir fechas a cadenas en Python: https://www.journaldev.com/23365/python-string-to-datetime-strptime