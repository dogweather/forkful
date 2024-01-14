---
title:                "Python: Borrando caracteres que coinciden con un patrón"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##Por qué

Eliminar caracteres que coincidan con un patrón es una técnica útil para el procesamiento de cadenas de texto en Python. Puede ser útil para limpiar datos o manipular cadenas de texto de manera eficiente.

##Cómo hacerlo

Para eliminar caracteres que coincidan con un patrón en Python, podemos utilizar la función `re.sub()` del módulo `re`. Esta función reemplaza todas las coincidencias del patrón con un carácter específico o una cadena de texto.

```Python
import re

# Definir una cadena de texto con caracteres que deseamos eliminar
cadena = "Esto es una cadena con caracteres # no deseados !"

# Utilizar la función re.sub() para eliminar todos los caracteres que coincidan con el patrón
limpiada = re.sub(r"[\#\!\&]", "", cadena) # El patrón que utilizamos es una expresión regular que incluye los caracteres que deseamos eliminar

# Imprimir la cadena limpiada
print(limpiada)

# El resultado será "Esto es una cadena con caracteres no deseados"
```

En este ejemplo, utilizamos una expresión regular para definir el patrón que queremos eliminar. Podemos adaptar esta expresión regular para que se ajuste a nuestras necesidades específicas.

##Profundizando

La función `re.sub()` en realidad utiliza la función `re.subn()` debajo para realizar la sustitución. Esta función devuelve una tupla con la cadena limpiada y el número de sustituciones realizadas. Podemos utilizar esta segunda función si necesitamos conocer el número de veces que se ha realizado la sustitución.

También es importante tener en cuenta que en Python 3, la función `re.sub()` devuelve un objeto de tipo `re.Match` en lugar de una cadena de texto, por lo que puede ser necesario aplicar el método `group()` a este objeto para obtener la cadena limpiada.

##Ver también

- Documentación oficial de Python para expresiones regulares: https://docs.python.org/es/3.9/library/re.html#module-re
- Tutorial sobre expresiones regulares en Python: https://realpython.com/regex-python/
- Documentación oficial de `re.sub()` en la biblioteca estándar de Python: https://docs.python.org/es/3.9/library/re.html#re.sub