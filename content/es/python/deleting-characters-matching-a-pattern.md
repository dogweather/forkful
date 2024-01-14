---
title:    "Python: Eliminando caracteres que coinciden con un patrón"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué borrar caracteres que coinciden con un patrón?
Eliminar caracteres que coinciden con un patrón puede ser útil para limpiar datos o para aplicar reglas específicas en un programa de Python. Esta es una tarea común en el procesamiento de texto y puede ayudar a mejorar la calidad y precisión de nuestros datos.

## Cómo hacerlo
Para borrar caracteres que coinciden con un patrón en Python, podemos utilizar la función `re.sub()` del módulo de expresiones regulares. Veamos un ejemplo en el que eliminamos todos los números de una cadena de texto:

```Python
import re

texto = "¡H0la! ¿C0m0 están?"

nuevo_texto = re.sub('[0-9]', '', texto)

print(nuevo_texto)
```

Output:
```Python
¡Hola! ¿Cómo están?
```

En este ejemplo, utilizamos una expresión regular `[0-9]` para indicar que queremos eliminar todos los caracteres numéricos del texto. La función `re.sub()` reemplaza el patrón encontrado con una cadena vacía, logrando así eliminar los números del texto.

También podemos utilizar expresiones regulares para eliminar otros tipos de caracteres, como signos de puntuación o espacios en blanco. Estas expresiones regulares se pueden encontrar en la documentación oficial de Python o en línea.

## Profundizando
Eliminar caracteres que coinciden con un patrón puede implicar mucho más que solo utilizar expresiones regulares. Dependiendo de nuestro programa, puede que necesitemos aplicar diferentes reglas y condiciones antes de eliminar los caracteres deseados. También podemos utilizar otras funciones y métodos de Python para manipular y limpiar nuestros datos. Por ejemplo, podemos utilizar la función `strip()` para eliminar espacios en blanco al inicio y al final de una cadena de texto.

Es importante entender cómo funcionan las expresiones regulares y conocer las diferentes opciones que tenemos para trabajar con ellas. De esta manera, podemos aplicarlas de manera efectiva y ahorrar tiempo y esfuerzo en nuestro código.

## Ver también
- [Documentación oficial de re.sub() en Python](https://docs.python.org/es/3/library/re.html)
- [Tutorial de Expressions Regulares en Python (en inglés)](https://realpython.com/regex-python/)
- [Lista de expresiones regulares útiles para limpiar datos en Python (en inglés)](https://www.analyticsvidhya.com/blog/2021/06/regular-expression-in-python-for-data-science/)