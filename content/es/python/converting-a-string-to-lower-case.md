---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Python: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
La conversión de una cadena de texto a minúsculas es un proceso en el que todas las letras mayúsculas de una cadena se convierten a su equivalente en minúsculas. Los programadores suelen hacer esto para estandarizar las entradas de los usuarios y facilitar la manipulación de cadenas de texto.

## Cómo hacerlo:
```Python
str = "HOLA MUNDO"
print(str.lower())

# Output: hola mundo
```
En este ejemplo, utilizamos el método `lower()` para convertir la cadena `str` a minúsculas. Este método es aplicable a cualquier cadena de texto en Python.

## In-depth:
El método `lower()` es una función de la clase `str` en Python. Fue introducido en la versión 1.5.2 de Python y desde entonces ha sido una función muy útil para manipular cadenas de texto.

Una alternativa a este método es utilizar la función `swapcase()` que convierte las letras mayúsculas a minúsculas y viceversa. Sin embargo, esta función no es tan utilizada ya que no permite estandarizar la entrada de los usuarios.

El proceso de conversión de una cadena de texto a minúsculas se basa en los valores de los caracteres en la tabla ASCII. Utilizando esta tabla, Python es capaz de identificar qué caracteres son mayúsculas y cuáles son minúsculas y realizar la conversión adecuada.

## Ver también:
Si quieres saber más sobre las funciones de manipulación de cadenas de texto en Python, puedes consultar la documentación oficial de Python en [https://docs.python.org/es/](https://docs.python.org/es/).

También puedes explorar otros métodos y funciones relacionadas como `upper()` para convertir una cadena a mayúsculas o `capitalize()` para convertir la primera letra de una cadena a mayúscula. ¡Diviértete explorando las posibilidades que ofrece Python para manipular cadenas de texto!