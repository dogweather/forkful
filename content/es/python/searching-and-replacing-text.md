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

# ¿Qué y por qué?
Buscar y reemplazar texto es un proceso común en la programación que consiste en encontrar y cambiar una palabra o conjunto de palabras por otra en un texto determinado. Los programadores lo hacen para ahorrar tiempo y evitar errores al realizar cambios en su código o en grandes cantidades de texto.

# Cómo hacerlo:
```
Python
# Usando el método replace() para reemplazar una palabra en una cadena de texto
cadena = "Hola mundo!"
nueva_cadena = cadena.replace("mundo", "Python")
print(nueva_cadena)
# Salida: Hola Python!
```

```
Python
# Utilizando expresiones regulares (regex) para realizar búsquedas y reemplazos avanzados
import re

cadena = "Hola mundo!"
nueva_cadena = re.sub(r"mundo", "Python", cadena)
print(nueva_cadena)
# Salida: Hola Python!
```

# Profundizando:
Buscar y reemplazar texto se ha vuelto más fácil a lo largo de los años gracias a los avances en tecnología y los lenguajes de programación. Antiguamente, los programas de búsqueda y reemplazo eran limitados y requerían mucho tiempo y esfuerzo para llevar a cabo cambios simples. Sin embargo, con el uso de regex y otros métodos, los programadores ahora pueden realizar búsquedas y reemplazos de manera más eficiente en grandes cantidades de texto. Alternativamente, también pueden utilizar editores de texto con esta funcionalidad incorporada.

# Ver también:
- Documentación oficial de Python: https://docs.python.org
- Expresiones regulares en Python: https://docs.python.org/3/library/re.html
- Editores de texto recomendados para programar: https://www.fullstackpython.com/blog/best-ide-text-editor-python.html