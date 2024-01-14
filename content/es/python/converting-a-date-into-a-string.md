---
title:                "Python: Transformando una fecha en una cadena"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena?

La conversión de una fecha en una cadena es una práctica común en la programación de Python. Permite a los desarrolladores manipular y manejar fechas y horarios de manera más eficiente en sus aplicaciones. Además, al convertir una fecha en una cadena, se puede formatear de acuerdo a las necesidades del usuario.

## Cómo hacerlo

Para convertir una fecha en una cadena en Python, se puede utilizar el método `strftime()` de la clase `datetime`. Este método toma una cadena de formato como argumento y devuelve la fecha en la cadena especificada.

```Python
from datetime import datetime

# Obtener la fecha actual
today = datetime.now()

# Convertir la fecha en una cadena con formato
date_str = today.strftime("%d/%m/%Y")

print(date_str) # Salida: 27/07/2021
```

Otra forma de convertir una fecha en una cadena es utilizando el módulo `calendar`. Este módulo contiene una función `month()` que toma un número de mes como argumento y devuelve el nombre del mes en formato de cadena.

```Python
import calendar

# Obtener el nombre del mes actual
month_str = calendar.month(7)

print(month_str) # Salida: Julio
```

## Profundizando en la conversión de fechas a cadenas

Cuando se convierte una fecha en una cadena, también se pueden especificar diferentes formatos para el día, el mes y el año. Por ejemplo, el formato `%d` representa el día en dos dígitos, mientras que `%m` representa el mes en dos dígitos. Se pueden consultar otras opciones de formato en la documentación de `strftime()`.

Además, al convertir una fecha en una cadena, se pueden agregar caracteres especiales para separar los elementos de la fecha. Por ejemplo, `"/"` se utiliza comúnmente como separador en fechas, pero se pueden utilizar otros caracteres como `-` o `.` según se prefiera.

## Ver también

- [Documentación de strftime() en Python](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes)
- [Documentación de calendar en Python](https://docs.python.org/3/library/calendar.html)
- [Tutorial de Python para principiantes en Español](https://realpython.com/python-tutorial-es/#por-qu%C3%A9-aprender-python)