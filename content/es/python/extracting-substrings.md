---
title:                "Extracción de Subcadenas"
html_title:           "Python: Extracción de Subcadenas"
simple_title:         "Extracción de Subcadenas"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas en Python?

Extraer subcadenas en Python puede ser una tarea bastante común en la programación. Esto se debe a que muchas veces necesitaremos trabajar solo con una parte de una cadena de texto determinada. Por ejemplo, si queremos extraer un nombre de un correo electrónico completo o si deseamos obtener solo el código de un número de identificación.

## Cómo extraer subcadenas en Python

Extraer subcadenas en Python es bastante sencillo utilizando la función incorporada `slice()` o mediante el uso de la sintaxis de indexación de cadenas.

```Python
# Utilizando la función slice()
cadena = "Este es un ejemplo de una cadena de texto."
subcadena = slice(5, 14) # Selecciona desde el índice 5 hasta el 13
print(cadena[subcadena]) # Imprime "es un ejem"

# Utilizando la sintaxis de indexación
cadena = "123456789"
print(cadena[2:8]) # Imprime "345678"
```

También podemos utilizar el método `split()` para dividir una cadena en subcadenas basadas en un separador específico.

```Python
# Utilizando el método split()
cadena = "Ejemplo de texto, separado por comas"
subcadenas = cadena.split(", ")
print(subcadenas) # Imprime ['Ejemplo de texto', 'separado por comas']
```

## Profundizando en la extracción de subcadenas

Es importante tener en cuenta que el índice del primer carácter de una cadena es 0, y que el índice del último carácter es -1. Además, podemos utilizar números negativos en nuestras subcadenas para contar desde el final de la cadena hacia atrás.

```Python
cadena = "¡Hola, mundo!"
print(cadena[1:]) # Desde el índice 1 hasta el final. Imprime "Hola, mundo!"
print(cadena[:-1]) # Desde el inicio hasta el índice -1. Imprime "¡Hola, mundo"
print(cadena[::-1]) # Imprime la cadena en orden inverso. "¡odnum ,aloh¡"
```

También podemos utilizar expresiones regulares para realizar búsquedas y reemplazos más complejos en nuestras cadenas.

```Python
import re
cadena = "¡Hola, Python!"
nueva_cadena = re.sub("[aeiou]", "¡", cadena)
print(nueva_cadena) # Imprime "¡H¡l¡, P¡th¡n!"
```

## Ver también

- Documentación oficial de Python sobre la función `slice()`: https://docs.python.org/3/library/functions.html#slice
- Tutorial de Real Python sobre manipulación de cadenas en Python: https://realpython.com/python-strings/