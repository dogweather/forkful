---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Obtener la fecha actual en Python significa saber exactamente qué día es hoy de acuerdo al sistema. Los programadores hacen esto por razones como registro de transacciones, timestamp de eventos o cálculos de tiempo.

## ¿Cómo hacerlo?:
Aquí se muestra cómo obtener la fecha actual utilizando la biblioteca `datetime` en Python:

```Python
import datetime

# Obtener la fecha y hora actuales
now = datetime.datetime.now()

# Imprimir la fecha actual
print("La fecha actual es: ", now)
```

Y aquí está la muestra de la salida:

```Python 
La fecha actual es:  2022-05-15 09:24:55.850674
```

## Profundización:
Luego de sumergirse en la biblioteca `datetime`, esto es lo que necesitas saber.

**Contexto histórico**: antes de la biblioteca `datetime`, los programadores a menudo lidiaban con la fecha y hora en Python a un nivel muy bajo. Con la introducción de `datetime` en Python 2.3 en 2003, trabajar con fechas y horas se volvió mucho más natural y fácil.

**Alternativas**: también puedes utilizar la biblioteca `time` para trabajar con fechas y horas.

```Python
import time

# Obtén el tiempo actual en segundos desde la Época de Unix
seconds = time.time()

# Conviértelo en una estructura de tiempo y luego imprímelo
print("La fecha actual es: ", time.ctime(seconds))
```

**Detalles de implementación**: `datetime.datetime.now()` devuelve la fecha y hora actuales. `datetime` provee tanto la fecha como la hora, por lo que es más versátil que `time.time()`, que sólo proporciona la hora.

## Ver también:
Para más información, revisa estos recursos:

- Documentación oficial de Python para `datetime`: [link](https://docs.python.org/3/library/datetime.html)
- Otros módulos en Python para trabajar con fecha y hora: `time`, `calendar`: [link](https://docs.python.org/3/library/time.html), [link](https://docs.python.org/3/library/calendar.html)
- Tutorial de Python sobre el trabajo con fecha y hora: [link](https://www.w3schools.com/python/python_datetime.asp)