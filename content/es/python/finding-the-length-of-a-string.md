---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Encontrar la longitud de una cadena significa contar el número de caracteres en esa cadena. Los programadores lo hacen porque a menudo necesitan, por ejemplo, validar entradas de usuarios o dividir cadenas en ciertos puntos.

## ¿Cómo hacerlo?:

En Python, puedes usar la función incorporada `len()` para encontrar la longitud de una cadena. Aquí te muestro cómo:

```Python
mi_cadena = "Hola Mundo"
print(len(mi_cadena))
```

Salida:

```Python
10
```

Como ves, la cadena "Hola Mundo" tiene 10 caracteres, incluido el espacio.

## Inmersión Profunda:

Históricamente, Python ha estado utilizando la función `len()` desde su primera versión lanzada en 1991. `len()` es una función nativa de Python, lo que significa que está incorporada y no necesitas importar ninguna biblioteca para usarla.

Alternativamente, puedes usar un ciclo for para contar el número de caracteres en una cadena:

```Python
mi_cadena = "Hola Mundo"
contador = 0

for caracter in mi_cadena:
    contador += 1
    
print(contador)
```

Esta implementación da también como resultado `10`, porque estamos incrementando el valor del contador en `1` para cada carácter en la cadena.

Por último, ten en cuenta que en Python, los caracteres especiales como los espacios, tabulaciones (`\t`) y saltos de línea (`\n`) se cuentan como un caracter.

## Ver También:

- Documentación oficial de Python: https://docs.python.org/3/library/functions.html#len
- Guía de inicio rápido de Python: https://docs.python.org/3/tutorial/introduction.html#strings
- Cadenas en Python: https://realpython.com/python-strings/