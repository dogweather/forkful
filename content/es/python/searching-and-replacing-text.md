---
title:                "Buscando y reemplazando texto"
html_title:           "Python: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué
Todos sabemos lo tedioso que puede ser reemplazar repetidamente el mismo texto en un documento o archivo. Afortunadamente, Python ofrece una solución eficiente para automatizar este proceso.

## Cómo hacerlo
Reemplazar texto con Python es bastante sencillo. Primero, importamos la librería *re* para utilizar sus funciones de búsqueda y reemplazo.

```Python
import re
```

Luego, definimos nuestra cadena de texto y el patrón que queremos reemplazar. En este ejemplo, reemplazaremos todas las letras "a" con la letra "e".

```Python
text = "Quiero aprender a programar en Python"
pattern = "a"
```

Finalmente, utilizamos la función *sub()* para realizar el reemplazo y guardamos el resultado en una nueva variable.

```Python
new_text = re.sub(pattern, "e", text)
```

¡Y listo! Ahora podemos imprimir nuestra nueva cadena de texto para verificar el resultado.

```Python
print(new_text)
```

La salida sería: "Quiero emprender e programer en Python". Podemos ver que todas las letras "a" han sido reemplazadas por la "e".

## Profundizando
Además de reemplazar un patrón específico, Python también ofrece la posibilidad de utilizar expresiones regulares para una búsqueda y reemplazo más avanzada.

Por ejemplo, si tenemos una lista de nombres con el formato "Apellido, Nombre" y queremos cambiarlo a "Nombre Apellido", podemos utilizar una expresión regular para lograrlo.

```Python
names = ["García, María", "Pérez, Juan", "Díaz, Ana"]
pattern = re.compile(r"(\w+), (\w+)")
```

Utilizando la función *sub()* podemos indicar cómo queremos que se vea la nueva cadena de texto. En este caso, intercambiaremos el orden de las palabras y añadiremos una coma y un espacio para mantener el mismo formato.

```Python
new_names = [pattern.sub(r"\2 \1,", name) for name in names]
```

La salida sería: ["María García,", "Juan Pérez,", "Ana Díaz,"].

Como podemos ver, el uso de expresiones regulares nos permite realizar reemplazos más complejos en una sola línea de código.

## Ver también
- Documentación oficial de Python sobre expresiones regulares: https://docs.python.org/es/3/library/re.html
- Tutorial de regex en español: https://www.stefanocudini.com/regular-expression-tutorial/
- Ejemplos prácticos de búsqueda y reemplazo con Python: https://www.geeksforgeeks.org/python-replace-all-occurrences-of-substring-in-string/