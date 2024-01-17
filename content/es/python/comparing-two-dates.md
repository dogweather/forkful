---
title:                "Comparando dos fechas"
html_title:           "Python: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Comparar dos fechas en la programación es una práctica común que consiste en evaluar si una fecha es anterior, posterior o igual a otra fecha. Los programadores lo hacen para ordenar y clasificar eventos, determinar el tiempo transcurrido entre dos fechas o verificar la validez de una fecha en una aplicación.

## Cómo hacerlo:
Comparar dos fechas en Python es sencillo gracias a los métodos integrados en la clase `datetime`. A continuación, se muestran ejemplos de código y su salida correspondiente.

```
import datetime

# Crear dos objetos de tipo datetime, uno con una fecha anterior y otro con una fecha posterior
fecha1 = datetime.datetime(2020, 5, 1)
fecha2 = datetime.datetime(2021, 3, 15)

# Comparación utilizando el operador "menor que"
if fecha1 < fecha2:
    print("La fecha 1 es anterior a la fecha 2")

# Comparación utilizando el método "compare"
if fecha1.compare(fecha2) < 0:
    print("La fecha 1 es anterior a la fecha 2")

# Salida:
# La fecha 1 es anterior a la fecha 2
# La fecha 1 es anterior a la fecha 2
```

## Paso a paso:
- Importa el módulo `datetime`.
- Utiliza el método `datetime` para crear dos objetos con las fechas que deseas comparar.
- Utiliza el operador `menor que` para evaluar si una fecha es anterior a otra, o utiliza el método `compare` para obtener un valor numérico en la comparación.
- Puedes utilizar otros operadores lógicos como `mayor que` o `igual que` para obtener diferentes resultados.

## Profundizando:
- La comparación de fechas se ha vuelto más sencilla gracias a los métodos integrados en la clase `datetime` en comparación con versiones anteriores de Python.
- En el caso de fechas con diferentes formatos, es necesario hacer una conversión para poder compararlas.
- Existen módulos de terceros que ofrecen métodos más avanzados para comparar fechas, como `arrow` o `dateutil`.

## Ver también:
- Documentación oficial de Python sobre fechas y tiempos: https://docs.python.org/es/3/library/datetime.html
- Tutorial de W3Schools sobre el manejo de fechas en Python: https://www.w3schools.com/python/python_datetime.asp