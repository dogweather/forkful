---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Cambiando y Buscando Texto en Python

## ¿Qué y Por Qué?
Cambiar y buscar texto son operaciones comunes en la programación. Te permiten encontrar cadenas de texto específicas y reemplazarlas con otras, lo que es útil para la manipulación de datos y la automatización de procesos.

## ¿Cómo hacerlo?

Usamos la función `replace()` en Python. Esta función toma dos argumentos, el texto a buscar y el texto por el cual se va a reemplazar.

```Python 
texto = "Hola Python!"
nuevo_texto = texto.replace("Python", "Mundo")
print(nuevo_texto)
```

La salida será:

```Python 
"Hola Mundo!"
```

## Deep Dive

La función `replace()` en python es parte de una biblioteca mucho mayor de métodos de cadenas de texto. Esta biblioteca ha evolucionado con Python desde sus primeras versiones.

A pesar de su utilidad, `replace()` podría no ser la mejor opción en algunas situaciones. Por ejemplo, si estás buscando coincidencias más complejas, las expresiones regulares (regex) podrían ser una mejor opción. El módulo `re` en Python proporciona funciones para trabajar con regex.

```Python 
import re
texto = "Hola Python!"
nuevo_texto = re.sub('Python', 'Mundo', texto)
print(nuevo_texto)
```
La salida será igualmente: 

```Python
"Hola Mundo!"
```

Incluso podríamos usar list comprehension para hacer reemplazos en Python.

```Python 
texto = "Hola Python!"
nuevo_texto = ''.join(['Mundo' if i=='Python' else i for i in texto.split()])
print(nuevo_texto)
```
De nuevo, la salida será:

```Python 
"Hola Mundo!"
```

## Ver También

Para obtener más información sobre la manipulación de cadenas de texto en Python, consulte los siguientes enlaces:

- Tutorial oficial de Python sobre texto: https://docs.python.org/es/3/tutorial/introduction.html#strings
- Documentación oficial de Python sobre el módulo `re`: https://docs.python.org/es/3/library/re.html