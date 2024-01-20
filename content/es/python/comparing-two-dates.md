---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qué y por qué?
Comparar dos fechas es determinar si una fecha es mayor, menor o igual a la otra. Los programadores suelen hacerlo para implementar funciones relacionadas con el tiempo, como temporizadores, calendarios y recordatorios.

## Cómo se hace:
```Python
from datetime import datetime

# dos fechas para comparar
fecha1 = datetime(2022, 5, 15)
fecha2 = datetime(2021, 5, 15)

# Comparando las fechas
if fecha1 > fecha2:
    print("La fecha1 es posterior a la fecha2")
elif fecha1 < fecha2:
    print("La fecha1 es anterior a la fecha2")
else:
    print("Ambas fechas son iguales")

# Salida:
# 'La fecha1 es posterior a la fecha2'
```

## Profundizando
**Contexto histórico**: En las primeras versiones de Python, comparar fechas podía ser un poco engorroso porque los programadores tenían que tomar cada fragmento de la fecha (año, mes, día, etc.) y compararlos individualmente.

**Alternativas**: Existen múltiples formas de comparar las fechas en Python, algunas librerías adicionales como Pandas y Numpy también tienen sus propios métodos.

**Detalles de implementación**: La comparación de fechas en Python es directa debido a la sobrecarga de operadores en los objetos `datetime`. Python internamente lo lleva a cabo comparando las fechas representadas en formato POSIX (segundos transcurridos desde 01/01/1970).

## Ver también
- Documentación oficial de Python para [`datetime`](https://docs.python.org/3/library/datetime.html)
- [Python Dates](https://www.w3schools.com/python/python_datetime.asp) (tutorial de W3Schools)
- [Comparar fechas](https://stackoverflow.com/questions/4659702/comparing-two-date-objects-in-python/4659732) (Stack Overflow)