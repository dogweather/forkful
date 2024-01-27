---
title:                "Uso de expresiones regulares"
date:                  2024-01-19
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Las expresiones regulares son secuencias de caracteres que forman un patrón de búsqueda, principalmente utilizadas para encontrar y manipular strings. Los programadores las usan para simplificar tareas complejas de búsqueda y reemplazo en textos, ahorrando tiempo y esfuerzo.

## Cómo Hacerlo:
```Bash
# Buscar todos los archivos que contengan "usuario"
grep "usuario" *.txt

# Reemplazar todas las instancias de "http" por "https" en un archivo
sed -i 's/http/https/g' archivo.txt

# Extraer números de un archivo de texto
grep -o '[0-9]\+' datos.txt

# Validar correos electrónicos en una lista
grep -E "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6}" emails.txt
```
Output ejemplo:
```
usuario1.txt:Data usuario
usuario2.txt:Otra línea con usuario
```

## Inmersión Profunda:
Las expresiones regulares tienen sus raíces en la teoría de autómatas y lenguajes formales. Ken Thompson incorporó esta capacidad en el editor QED y más tarde en Unix, lo que legó su uso a herramientas como grep, sed, y awk. Alternativas modernas incluyen librerías de expresiones regulares en lenguajes de alto nivel como Python y Java, cada una con sus propias particularidades de implementación. En Bash, las expresiones regulares son menos potentes que en lenguajes especializados, pero son suficientes para muchas tareas comunes de scripting.

## Ver También:
- Tutorial de grep: https://www.gnu.org/software/grep/manual/grep.html
- Sed & Awk 101: https://www.gnu.org/software/sed/manual/sed.html, https://www.gnu.org/software/gawk/manual/gawk.html
- Historia de las expresiones regulares: https://www.regular-expressions.info/history.html
- Prueba de patrones de expresiones regulares en línea: https://regexr.com/
