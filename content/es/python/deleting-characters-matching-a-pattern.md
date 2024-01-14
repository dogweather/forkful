---
title:    "Python: Eliminando caracteres que coinciden con un patrón"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por qué 
A veces, en programación, es necesario eliminar caracteres que coinciden con un patrón específico en una cadena de texto. Este proceso puede ser útil en situaciones como limpieza de datos o análisis de texto. En este artículo, exploraremos cómo se puede hacer esto en Python.

## Cómo hacerlo
Para eliminar caracteres que coinciden con un patrón, utilizaremos el método "sub" de la librería "re" (regular expressions) en Python. Primero, debemos importar la librería:

```Python
import re
```

A continuación, definiremos una cadena de texto que contenga el patrón que deseamos eliminar:

```Python
texto = "¡Hola! ¿Cómo estás? ¡Hola!"
```

Para eliminar el patrón "¡Hola!", podemos utilizar el siguiente código:

```Python
texto = re.sub(r"¡Hola!", "", texto)
print(texto)
```

La salida será: " ?Cómo estás? ". Como puedes ver, hemos eliminado todas las instancias del patrón "¡Hola!" de la cadena de texto.

También podemos utilizar expresiones regulares más complejas para eliminar patrones específicos. Por ejemplo, si deseamos eliminar todos los dígitos de una cadena, podemos hacerlo de la siguiente manera:

```Python
cadena = "A1B2C3D4"
cadena = re.sub(r"\d+", "", cadena)
print(cadena)
```

La salida será: "ABCD", ya que hemos eliminado los dígitos de la cadena.

## Profundizando
El método "sub" de la librería "re" nos permite reemplazar los caracteres que coinciden con un patrón en una cadena de texto con una cadena vacía. Pero también podemos utilizar grupos en expresiones regulares para reemplazar los patrones con otro texto. Por ejemplo:

```Python
texto = "Hola, mi nombre es Juan"
texto = re.sub(r"(\w+), (\w+) (\w+)", r"\3, \2 \1", texto)
print(texto)
```

La salida será: "Juan, nombre mi es Hola", ya que hemos utilizado grupos para reordenar las palabras en la cadena de texto.

También podemos utilizar el método "sub" para reemplazar el patrón con una versión modificada del mismo patrón. Por ejemplo, si queremos eliminar todos los caracteres de una cadena excepto las letras, podemos hacer lo siguiente:

```Python
cadena = "A1B2C3D4"
cadena = re.sub(r"[^a-zA-Z]", "", cadena)
print(cadena)
```

La salida será: "ABCD", ya que hemos reemplazado todos los caracteres que no son letras con una cadena vacía.

## Ver también
- [Documentación de la librería re de Python](https://docs.python.org/es/3/library/re.html)
- [Tutorial de expresiones regulares en Python](https://realpython.com/regex-python/)