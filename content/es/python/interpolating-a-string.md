---
title:                "Interpolación de cadenas de texto"
date:                  2024-01-20T17:51:38.293210-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolación de cadenas de texto"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
La interpolación de cadenas permite insertar valores de variables directamente dentro de una cadena de texto. Los programadores la usan para crear mensajes dinámicos, simplificando el código y mejorando su legibilidad.

## Cómo Hacerlo:
Python tiene varias formas de interpolar cadenas, pero el método preferido hoy es usar f-strings, introducido en la versión 3.6. Aquí hay ejemplos.

```Python
nombre = "Miguel"
edad = 30

# Uso de f-strings
saludo = f"Hola, mi nombre es {nombre} y tengo {edad} años."
print(saludo)
```
Output:
```
Hola, mi nombre es Miguel y tengo 30 años.
```

Alternativamente, podrías usar el método format():
```Python
saludo_format = "Hola, mi nombre es {} y tengo {} años.".format(nombre, edad)
print(saludo_format)
```
Output:
```
Hola, mi nombre es Miguel y tengo 30 años.
```

## A Fondo:
Antes de Python 3.6, los f-strings no existían y los programadores usaban el método `format()` o la concatenación. Concatenar es menos eficiente y más propenso a errores, especialmente con múltiples variables. La interpolación de cadenas mediante f-strings es más rápida y legible, ya que incluye la variable directamente en el lugar deseado.

Las f-strings son también expresiones evaluadas en tiempo de ejecución, lo que significa que puedes incluir cualquier expresión de Python válida dentro de las llaves:

```Python
print(f"En cinco años, {nombre} tendrá {edad + 5} años.")
```
Output:
```
En cinco años, Miguel tendrá 35 años.
```

Aunque los f-strings son preferidos por su claridad y eficiencia, en contextos donde la seguridad es una preocupación significativa (por ejemplo, cuando se interpolan cadenas proporcionadas por el usuario), se debe tener cuidado, ya que pueden ser susceptibles a inyecciones de código si no se manejan adecuadamente.

## Ver También:
- PEP 498 que introdujo los f-strings: https://www.python.org/dev/peps/pep-0498/
- Documentación oficial de Python sobre f-strings: https://docs.python.org/3/reference/lexical_analysis.html#f-strings
- Tutorial sobre formateo de cadenas en Python: https://realpython.com/python-f-strings/
