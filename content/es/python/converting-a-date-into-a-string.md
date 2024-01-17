---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Python: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Convertir una fecha (date) en una cadena (string) es un proceso común en la programación. Esencialmente, se trata de tomar una fecha en un formato específico y convertirla en una serie de caracteres que se puedan almacenar, manipular y mostrar fácilmente en un programa.

Los programadores realizan esta acción para poder trabajar con fechas de manera más eficiente. Por ejemplo, pueden tener una lista de fechas en una base de datos que necesiten mostrar en un formato específico en una aplicación web. Convertir estas fechas en cadenas les permite mostrarlas en el formato deseado sin perder información.

## Cómo:

```Python
# Importar el módulo datetime del paquete datetime
from datetime import datetime

# Definir una fecha en el formato YYYY-MM-DD
fecha = "2021-10-01"

# Convertir la fecha en una cadena con el formato DD/MM/YYYY
fecha_cadena = datetime.strptime(fecha, "%Y-%m-%d").strftime("%d/%m/%Y")

# Imprimir la fecha convertida
print(fecha_cadena)

# Output: 01/10/2021
```

## Inmersión Profunda:

Históricamente, antes de que los lenguajes de programación fueran capaces de manejar fechas directamente, los programadores tenían que convertir las fechas a un formato en el que pudieran trabajar. Esto se hacía a menudo con la ayuda de funciones especiales llamadas "rutinas de tiempo" (time routines).

Una alternativa a la conversión de fechas en cadenas es almacenarlas directamente como objetos de fecha en el código. Sin embargo, esto puede ser más complejo y menos eficiente en términos de manipulación y visualización de las fechas.

## Ver También:

Documentación oficial de Python sobre el módulo datetime: https://docs.python.org/es/3/library/datetime.html