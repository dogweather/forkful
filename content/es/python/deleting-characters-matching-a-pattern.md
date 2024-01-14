---
title:                "Python: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Eliminar caracteres que coinciden con un patrón es una habilidad esencial en la programación de Python para limpiar y procesar datos. Puede ser útil cuando se trabaja con cadenas de texto o cuando se necesita analizar información de una fuente externa.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Python, se debe utilizar el método `sub()` de la librería `re` (expresiones regulares). Por ejemplo, si queremos eliminar todos los espacios en blanco de una cadena de texto, podemos usar el siguiente código:

```Python
import re

texto = "Hola, esto es una cadena de texto con espacios en blanco"
nuevo_texto = re.sub(r"\s+", "", texto)
print(nuevo_texto)
```

El resultado de este código será: `Hola,estoesunacadenadetextoconespaciosenblanco`. Aquí, utilizamos la expresión regular `\s+`, que representa uno o más espacios en blanco a ser reemplazados por una cadena vacía `""`.

También se pueden utilizar expresiones regulares más complejas para eliminar caracteres que coinciden con un patrón específico. Por ejemplo, si queremos eliminar todos los caracteres que no sean letras o números de una cadena de texto, podemos usar el siguiente código:

```Python
import re

texto = "S0l0 5on pal4bras"
nuevo_texto = re.sub(r"[^\w\s]", "", texto)
print(nuevo_texto)
```

El resultado de este código será: `S0l0 5on pal4bras`. Aquí, utilizamos la expresión regular `[^\w\s]`, que representa cualquier carácter que no sea una letra, un número o un espacio en blanco, y lo reemplaza por una cadena vacía.

## Profundizando

Las expresiones regulares son una herramienta poderosa y versátil en Python para trabajar con cadenas de texto. Se pueden utilizar para buscar, reemplazar o eliminar patrones específicos en una cadena de texto de manera eficiente.

Para aprender más sobre expresiones regulares y su uso en Python, se puede consultar la documentación oficial de la librería `re` (https://docs.python.org/es/3/library/re.html) o seguir tutoriales y ejemplos prácticos en línea.

## Ver también

- La documentación oficial de la librería `re` (https://docs.python.org/es/3/library/re.html)
- Tutorial de expresiones regulares de Real Python (https://realpython.com/regex-python/)
- Video tutorial sobre expresiones regulares de Corey Schafer (https://youtu.be/K8L6KVGG-7o)