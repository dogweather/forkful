---
title:                "Conversión de una cadena de texto a minúsculas"
aliases: - /es/python/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:15.916928-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversión de una cadena de texto a minúsculas"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Convertir una cadena a minúsculas significa transformar todos los caracteres alfabéticos en dicha cadena a su equivalente en minúsculas. Los programadores lo hacen para unificar los datos, facilitando comparaciones y búsquedas sin que importe el caso de las letras.

## Cómo Hacerlo:

Python hace que convertir cadenas a minúsculas sea pan comido con el método `.lower()`. Aquí te muestro cómo usarlo:

```Python
original = "¡Hola Mundo!"
minuscula = original.lower()
print(minuscula)
```

Salida:
```
¡hola mundo!
```

Si trabajas con Unicode, el método `.casefold()` es más agresivo y puede ser más adecuado para tus necesidades:

```Python
original = "Straße"
minuscula = original.casefold()
print(minuscula)
```

Salida:
```
strasse
```

## Conociendo Más a Fondo:

**Historia:** El método `.lower()` ha estado en Python desde sus principios. Su necesidad nace de la complejidad del texto en informática; por ejemplo, la diferencia entre mayúsculas y minúsculas en sistemas de archivos y lenguajes de programación.

**Alternativas:** Además de `.lower()` y `.casefold()`, Python ofrece otras funciones de manipulación de cadenas como `.upper()`, `.capitalize()` y `.title()` para diferentes propósitos.

**Detalles de Implementación:** `.lower()` convertirá todos los caracteres de letras definidos en ASCII a minúsculas. Sin embargo, las cosas se ponen interesantes con Unicode, ya que algunos caracteres no tienen una forma directa de minúsculas o pueden transformarse en múltiples caracteres, lo que `.casefold()` maneja mejor.

## Ver También:

- Documentación oficial de Python sobre métodos de cadenas: https://docs.python.org/3/library/stdtypes.html#string-methods
- Exploración de Unicode y normalización de cadenas: https://docs.python.org/3/howto/unicode.html
- Python PEP 3131, que discute el soporte para caracteres no ASCII en nombres de identificadores de Python, y que toca temas sobre mayúsculas y minúsculas: https://www.python.org/dev/peps/pep-3131/
