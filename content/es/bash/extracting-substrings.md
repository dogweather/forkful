---
title:                "Extracción de subcadenas"
date:                  2024-01-20T17:45:10.137751-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracción de subcadenas"

category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Extraer subcadenas significa obtener partes específicas de una cadena de texto. Los programadores lo hacen para filtrar datos, manejar entradas del usuario, o simplemente porque necesitan usar solo una porción de información.

## Cómo hacerlo:
Extraer subcadenas es simple. Bash utiliza índices para obtener los fragmentos que necesitas. Aquí hay un par de ejemplos:

```Bash
# Extrae desde un índice hasta el final
cadena="Hola, esto es una cadena de texto"
echo ${cadena:7} # Devuelve 'esto es una cadena de texto'

# Extrae un rango específico [inicio:longitud]
echo ${cadena:7:4} # Devuelve 'esto'

# Extrae desde un índice negativo hasta el final
echo ${cadena: -5} # Devuelve 'texto'
```

Salida:
```
esto es una cadena de texto
esto
texto
```

## Análisis Profundo:
Extraer subcadenas es una funcionalidad que ha existido desde los primeros días de la programación. Bash, siendo un lenguaje de shell antiguo, incorpora esto con una sintaxis sencilla. Alternativas incluyen usar `awk`, `cut`, o `grep` para casos más complejos. Los detalles de implementación relatan cómo Bash maneja cadenas como arreglos de caracteres, lo que permite el uso del indexado para su manipulación.

## Ver También:
- GNU Bash Manual: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/string-manipulation.html
- Stack Overflow - Preguntas y respuestas sobre extracción de subcadenas en Bash: https://stackoverflow.com/questions/tagged/bash+substring
