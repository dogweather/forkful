---
title:                "Capitalizando una cadena"
html_title:           "Python: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué? 
Capitalizar una cadena significa hacer que la primera letra de cada palabra sea mayúscula. Los programadores suelen hacerlo para seguir buenas prácticas de codificación, facilitar la legibilidad del código y cumplir con ciertas convenciones de estilo.

## Cómo: 
Los programadores pueden capitalizar una cadena utilizando el método ```capitalize ()``` o el método ```title ()``` en Python.

```
# Ejemplo 1: Utilizando el método capitalize()
cadena = "hola, soy un programador"
print(cadena.capitalize())

# Output: Hola, soy un programador

# Ejemplo 2: Utilizando el método title()
cadena = "soy un desarrollador web"
print(cadena.title())

# Output: Soy Un Desarrollador Web
```

## Profundizando: 
En el pasado, la capitalización de las cadenas se utilizaba principalmente para el formateo de texto en aplicaciones de procesamiento de texto. Sin embargo, ahora se ha convertido en una práctica común entre los programadores debido a su impacto en la legibilidad y claridad del código.

Además de los métodos mencionados anteriormente, también es posible capitalizar una cadena utilizando la función ```upper()```, que convierte todas las letras en mayúsculas. También hay algunas bibliotecas de Python, como ```string.capwords()```, que se pueden utilizar para capitalizar automáticamente las palabras en una cadena.

## Ver también: 
- Documentación oficial de Python para los métodos ```capitalize()``` y ```title()```: https://docs.python.org/es/3/library/stdtypes.html#string-methods 
- PEP 8 - Guía de estilo para el código en Python: https://www.python.org/dev/peps/pep-0008/