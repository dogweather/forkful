---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:23:50.789079-07:00
description: "La interpolaci\xF3n de cadenas es el m\xE9todo de incrustar expresiones\
  \ dentro de literales de cadena. Los programadores lo utilizan para insertar valores\u2026"
lastmod: '2024-03-13T22:44:58.596503-06:00'
model: gpt-4-0125-preview
summary: "La interpolaci\xF3n de cadenas es el m\xE9todo de incrustar expresiones\
  \ dentro de literales de cadena. Los programadores lo utilizan para insertar valores\u2026"
title: Interpolando una cadena de caracteres
weight: 8
---

## Qué y Por Qué?
La interpolación de cadenas es el método de incrustar expresiones dentro de literales de cadena. Los programadores lo utilizan para insertar valores dinámicamente en las cadenas, lo que hace que el código sea más legible y limpio que la concatenación de cadenas tradicional.

## Cómo hacerlo:
En Python 3.6 y versiones superiores, puedes interpolar cadenas utilizando f-strings. Así es como se hace:

```Python
name = 'Alice'
age = 30
greeting = f"Hola, {name}. Tienes {age} años."

print(greeting)
```

Salida:
```
Hola, Alice. Tienes 30 años.
```

También puedes usar expresiones dentro de las llaves:

```Python
a = 5
b = 10
info = f"Cinco más diez es {a + b}, no {2 * (a + b)}."

print(info)
```

Salida:
```
Cinco más diez es 15, no 30.
```

## Análisis Profundo
Antes de Python 3.6, `.format()` era la forma de proceder para interpolar cadenas:

```Python
name = 'Bob'
age = 25
greeting = "Hola, {}. Tienes {} años.".format(name, age)

print(greeting)
```

El antiguo Python (versiones < 2.6) utilizaba el operador `%` para la interpolación, que es menos intuitivo y puede complicarse con múltiples variables:

```Python
name = 'Carol'
age = 35
greeting = "Hola, %s. Tienes %d años." % (name, age)

print(greeting)
```

Además de una sintaxis más limpia, las f-strings son más rápidas porque se evalúan en tiempo de ejecución y luego se convierten directamente en una operación de formato de cadena eficiente. El método `.format()` y el operador `%` involucran más pasos y son más lentos.

## Ver También
- [PEP 498 – Literal String Interpolation](https://www.python.org/dev/peps/pep-0498/) para la documentación oficial sobre f-strings.
- [Python f-strings](https://realpython.com/python-f-strings/) por Real Python para un tutorial sobre el uso de f-strings.
- [El Método .format()](https://docs.python.org/3/library/stdtypes.html#str.format) en la documentación de Python para entender el antiguo método `.format()` de formateo de cadenas.
