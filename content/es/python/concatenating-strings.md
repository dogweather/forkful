---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

La concatenación de cadenas implica unir dos o más cadenas de texto. Los programadores lo utilizan para combinar datos de texto o para generar salidas dinámicas.

## Cómo se hace:

Aquí te muestro cómo puedes concatenar cadenas en Python.

```Python
nombre = "Carlos"
saludo = "Hola, " + nombre
print(saludo)
```

La salida del ejemplo anterior sería:

```Python
>>> 'Hola, Carlos'
```

También puedes utilizar el método `format()`:

```Python
nombre = "Carlos"
saludo = "Hola, {}".format(nombre)
print(saludo)
```

La salida del ejemplo sería:

```Python
>>> 'Hola, Carlos'
```

## Un vistazo más profundo

**Contexto histórico:** En las primeras versiones de Python, solo se podía concatenar cadenas con el operador `+`. Sin embargo, a partir de Python 3.6, se pueden utilizar las F-strings para concatenar cadenas de manera más eficiente.

**Alternativas:** Las F-strings son una excelente alternativa para concatenar cadenas. Permiten incorporar variables directamente en la cadena de texto.

```Python
nombre = "Carlos"
saludo = f"Hola, {nombre}"
print(saludo)
```

La salida del ejemplo es:

```Python
>>> 'Hola, Carlos'
```

**Implementación:** La concatenación de cadenas es costosa en términos de memoria y rendimiento. Cada vez que se concatenan dos cadenas, Python crea una nueva cadena, lo que puede llevar a un gasto innecesario de memoria si se concatenan muchas cadenas.

## Ver también

Para más información sobre la concatenación de cadenas en Python, se puede consultar los siguientes enlaces:

- Documentación oficial de Python: https://docs.python.org/3/tutorial/introduction.html#strings
- Guía de Python sobre cadenas F-String: https://realpython.com/python-f-strings/
- Tutorial de Python sobre concatenación de cadenas: https://www.w3schools.com/python/gloss_python_string_concatenation.asp