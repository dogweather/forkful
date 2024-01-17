---
title:                "Interpolando una cadena"
html_title:           "Python: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Interpolar una cadena (string en inglés) es simplemente reemplazar parte de una cadena con valores de variables u otros datos. Los programadores lo hacen para crear cadenas personalizadas y dinámicas que incluyan información específica en diferentes contextos.

## ¿Cómo hacerlo?

Aquí hay dos métodos para interpolar una cadena en Python:

1. Método de formato de cadenas (string formatting):
```
nombre = "Melisa"
edad = 20
print("Hola, soy {0} y tengo {1} años".format(nombre, edad))
```
Salida: Hola, soy Melisa y tengo 20 años.

2. Método de concatenación:
```
nombre = "Juan"
edad = 25
print("Hola, soy " + nombre + " y tengo " + str(edad) + " años.")
```
Salida: Hola, soy Juan y tengo 25 años.

## Mira más a fondo

Interpolation strings en Python se popularizó con la introducción del método de formato de cadenas en la versión 2.6 y el uso de f-strings en la versión 3.6. Otra alternativa es el método de concatenación con la función str() para convertir los valores en cadenas.

Algunos otros lenguajes de programación también tienen métodos de interpolación de cadenas, como JavaScript con el método de plantillas de cadenas (template strings) y PHP con la función sprintf(). Además, existen módulos en Python que amplían las funcionalidades de interpolación de cadenas, como el módulo "string" en la biblioteca estándar.

## Consulta también

- [Documentación oficial de Python sobre cadena de formato](https://docs.python.org/es/3/library/string.html#formatstrings)
- [Guía de f-strings de Real Python](https://realpython.com/python-f-strings/)
- [Más información sobre plantillas de cadenas en JavaScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/template_strings)
- [Explicación detallada sobre la función sprintf() en PHP](https://www.php.net/manual/es/function.sprintf.php)