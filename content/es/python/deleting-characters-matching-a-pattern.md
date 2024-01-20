---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

**## ¿Qué y Por Qué?**

Eliminar caracteres que coinciden con un patrón permite limpiar nuestros datos en Python, excluyendo cualquier carácter innecesario o no deseado. Los programadores lo hacen para mejorar la precisión de los resultados y minimizar los errores.

**## Cómo hacerlo:**

Podemos utilizar la función `re.sub()` que ofrece la biblioteca `re` (regular expression) de Python para eliminar caracteres. Aquí hay algunos códigos de ejemplo:

```Python
import re

# Eliminar todos los números de una cadena
cadena = 'ABC123, ¡Hola Mundo!456'
resultado = re.sub('\d', '', cadena)
print(resultado) # Salida: ABC, ¡Hola Mundo!

# Eliminar todas las letras de una cadena
cadena = 'ABC123, ¡Hola Mundo!456'
resultado = re.sub('[a-zA-Z]', '', cadena)
print(resultado) # Salida: 123, !456
```

**## Inmersión profunda:**

Históricamente, las expresiones regulares se han utilizado ampliamente en ciencias de la computación para el manejo de cadenas de texto. Python proporciona la biblioteca `re` para trabajar con ellas.

Otra alternativa es usar el método `translate()` junto con `maketrans()`. Sin embargo, `re.sub()` es más flexible al trabajar con patrones en lugar de caracteres individuales.

Al utilizar `re.sub()`, Python compila la expresión regular y la utiliza para aplicar la sustitución de texto. Esto puede consumir recursos computacionales, así que si vas a hacerlo muchas veces, es recomendable precompilar la expresión regular con `re.compile()`.

**## Ver También:**

1. [Libreria re](https://docs.python.org/es/3/library/re.html) en la documentación oficial de Python.
2. [RegEx Module](https://www.w3schools.com/python/python_regex.asp) en W3Schools.
3. [Python - Eliminar caracteres no deseados](https://stackoverflow.com/questions/5843518/remove-all-special-characters-punctuation-and-spaces-from-string) en Stack Overflow.