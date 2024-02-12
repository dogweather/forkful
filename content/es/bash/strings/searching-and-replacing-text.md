---
title:                "Buscando y reemplazando texto"
aliases:
- /es/bash/searching-and-replacing-text/
date:                  2024-01-20T17:57:11.106535-07:00
model:                 gpt-4-1106-preview
simple_title:         "Buscando y reemplazando texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Buscar y reemplazar texto es cambiar una cadena por otra en un archivo o conjunto de archivos. Los programadores lo hacen para corregir errores, actualizar datos o modificar código de manera eficiente.

## Cómo:
```Bash
# Buscar y reemplazar "hola" por "adiós" en 'archivo.txt'
sed 's/hola/adiós/g' archivo.txt

# Cambios permanentes con la opción -i
sed -i 's/viejo/nuevo/g' archivo.txt

# Múltiples archivos con globbing
sed -i 's/error/corrección/g' *.log

# Mostrar solo las líneas modificadas
sed -n 's/texto1/texto2/p' archivo.txt
```

Ejemplo de salida:
```
# Antes de reemplazo en 'archivo.txt':
Una línea con hola y otra información.

# Después de reemplazo:
Una línea con adiós y otra información.
```

## Deep Dive
Originalmente, `sed` (stream editor) fue parte del proyecto de software de Unix desarrollado en los 70s. Es estupendo para scripts y para trabajar en archivos muy grandes porque procesa el texto línea por línea. Alternativas como `awk` y `perl` también son poderosas para manipular texto pero con enfoques y sintaxis diferentes. En la implementación, `sed` trabaja con patrones y expresiones regulares, lo que le permite realizar operaciones complejas de buscar y reemplazar.

## Ver También
- GNU sed manual: https://www.gnu.org/software/sed/manual/sed.html
- Expresiones regulares en profundidad: https://www.regular-expressions.info/
- Ejemplos de uso de `awk`: https://www.gnu.org/software/gawk/manual/gawk.html
