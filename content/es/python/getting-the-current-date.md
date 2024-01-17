---
title:                "Obteniendo la fecha actual"
html_title:           "Python: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Obtener la fecha actual es una tarea común en la programación. Significa obtener la fecha y hora actuales en el formato deseado. Los programadores suelen hacerlo para registrar el momento en que se ejecuta un programa o para mostrar la fecha actual en una aplicación.

## ¿Cómo hacerlo?

Puedes utilizar el módulo datetime en Python para obtener la fecha y hora actuales. Aquí hay un ejemplo sencillo que muestra la fecha actual en formato de día, mes y año:

```python
import datetime

fecha_actual = datetime.datetime.now()
print(fecha_actual.strftime("%d/%m/%Y"))
```

La salida sería algo como esto: `24/09/2021`.

## Profundizando

Este método para obtener la fecha actual en Python está disponible desde la versión 2.3 del lenguaje. Sin embargo, existen otras formas de hacerlo, como utilizando la biblioteca `time` o el módulo `calendar`.

El módulo datetime ofrece más funcionalidades para manipular fechas y horas, como la posibilidad de crear un objeto de fecha o hora específico en lugar de solo obtener la fecha y hora actuales. También se pueden realizar operaciones matemáticas con fechas y horas utilizando este módulo.

## Ver también

Puedes encontrar más información sobre el uso del módulo datetime en la documentación oficial de Python: https://docs.python.org/3/library/datetime.html

Si estás interesado en aprender más sobre cómo trabajar con fechas y horas en Python, puedes consultar este tutorial: https://www.programiz.com/python-programming/datetime-operations