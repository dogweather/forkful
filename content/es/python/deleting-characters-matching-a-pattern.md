---
title:                "Borrando caracteres que coinciden con un patrón"
html_title:           "Python: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Eliminar caracteres que coinciden con un patrón es una técnica utilizada por programadores para eliminar caracteres específicos de una cadena de texto. Esto es necesario cuando se trabaja con datos no deseados o en la limpieza de datos antes de procesarlos.

## ¿Cómo hacerlo?

Para eliminar caracteres que coinciden con un patrón en Python, se puede utilizar la función "re.sub()" del módulo "re". Por ejemplo, si queremos eliminar todas las vocales de una cadena de texto, podemos usar el siguiente código:

```Python
import re

texto = "Hola mundo"
texto_sin_vocales = re.sub("[aeiou]", "", texto)

print(texto_sin_vocales)
```

Esto imprimirá "Hl mnd" como resultado.

## Profundizando

Eliminar caracteres que coinciden con un patrón es una técnica común en programación y es especialmente útil en tareas de limpieza de datos. La función "re.sub()" de Python utiliza expresiones regulares para buscar y eliminar los caracteres que coinciden con el patrón especificado.

En lugar de usar la función "re.sub()", también se puede utilizar un bucle para recorrer la cadena de texto y eliminar manualmente cada caracter no deseado. Sin embargo, esto puede ser más tedioso y propenso a errores.

## Ver también

- [La documentación oficial de Python sobre el módulo "re"](https://docs.python.org/es/3/library/re.html)
- [Una guía completa de expresiones regulares en Python](https://realpython.com/regex-python/)
- [Ejemplos de uso de la función "re.sub()"](https://www.programiz.com/python-programming/regex)