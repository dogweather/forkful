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

## Por qué

A veces necesitamos comparar dos fechas para determinar qué tan diferentes son. Por ejemplo, podemos querer saber cuántos días han pasado desde nuestra última cita médica o cuánto tiempo ha transcurrido desde que comenzamos un nuevo trabajo. En este artículo, aprenderemos cómo comparar dos fechas en Python.

## Cómo hacerlo

Podemos comparar dos fechas en Python utilizando el operador de igualdad (==). Este operador devuelve un valor booleano (Verdadero o Falso) en función de si las fechas son iguales o no.

```Python
# Creamos dos variables con fechas
fecha1 = "2021-06-01"
fecha2 = "2021-06-15"

# Comparamos las fechas utilizando el operador de igualdad
if fecha1 == fecha2:
   print("Las fechas son iguales")
else:
   print("Las fechas son distintas")
```

La salida de este código será "Las fechas son distintas", ya que las fechas no son iguales.

También podemos comparar fechas utilizando otros operadores, como "mayor que" (>), "menor que" (<) o "mayor o igual que" (>=). Estos operadores nos permiten determinar el orden cronológico entre dos fechas.

```Python
# Creamos dos variables con fechas
fecha1 = "2021-06-01"
fecha2 = "2021-06-15"

# Comparamos las fechas utilizando el operador mayor que
if fecha1 > fecha2:
   print("La fecha 1 es posterior a la fecha 2")
else:
   print("La fecha 1 es anterior o igual a la fecha 2")
```

En este caso, la salida será "La fecha 1 es anterior o igual a la fecha 2".

## Profundizando

Las fechas en Python se pueden representar de diferentes maneras, como cadenas de texto (como en los ejemplos anteriores), objetos datetime o timestamps. Es importante asegurarse de que las fechas se estén comparando en el mismo formato para obtener resultados precisos.

Además, Python tiene un módulo de datetime incorporado que nos permite realizar operaciones y comparaciones más avanzadas con fechas. Puedes explorar más sobre este módulo en la documentación oficial de Python.

## Ver también

- Documentación Python sobre operadores de comparación: https://docs.python.org/es/3/reference/expressions.html#comparisons
- Documentación Python sobre el módulo datetime: https://docs.python.org/es/3/library/datetime.html