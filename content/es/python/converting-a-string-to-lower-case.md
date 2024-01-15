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

## Por qué

Puede ser útil convertir una cadena de texto a minúsculas al trabajar con datos de texto en un programa. Esto puede facilitar la comparación de cadenas y el procesamiento de datos en general.

## Cómo hacerlo

```Python
cadena = "Este es un EJEMPLO de cadena"
print(cadena.lower())
```
Output: este es un ejemplo de cadena

Para convertir una cadena a minúsculas en Python, se utiliza el método `lower()` en la variable que contiene la cadena. Este método devuelve la cadena original con todos los caracteres en minúscula.

## Profundizando

Además del método `lower()`, existen otras formas de convertir una cadena a minúsculas en Python. Una opción es utilizar la función `str.lower()` en lugar del método. Esta función también devuelve la cadena original con todos los caracteres en minúscula.

Otra forma de convertir una cadena a minúsculas es utilizando el módulo `string`. Este módulo proporciona una función `string.ascii_lowercase` que devuelve una cadena con todas las letras minúsculas del alfabeto inglés.

También se pueden utilizar operadores de comparación para comparar cadenas en minúsculas. Por ejemplo:
```Python
cadena1 = "hola"
cadena2 = "HOLA"
if cadena1.lower() == cadena2.lower():
  print("Las cadenas son iguales.")
```
Output: Las cadenas son iguales.

En este ejemplo, se utilizó el método `lower()` en ambas cadenas para compararlas en minúsculas y determinar si son iguales.

## Ver también

- [Python String Methods](https://www.w3schools.com/python/python_ref_string.asp) - Documentación oficial de Python sobre los métodos de cadena.
- [Convertir cadena a minúsculas en Python](https://www.educative.io/edpresso/how-to-convert-string-to-lowercase-in-python) - Tutorial detallado sobre cómo convertir una cadena a minúsculas en Python.
- [How to Change Strings to Lowercase - Python Tutorial](https://www.youtube.com/watch?v=NHg3oRRIVVg) - Video tutorial sobre cómo convertir una cadena a minúsculas en Python.