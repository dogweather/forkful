---
title:                "Usando expresiones regulares"
html_title:           "Go: Usando expresiones regulares"
simple_title:         "Usando expresiones regulares"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Exprésate Regularmente: Todo sobre Bash y las Expresiones Regulares

## ¿Qué & Por qué?

Las expresiones regulares son patrones usados para encontrar y manipular texto. Nos simplifican la vida a los programadores permitiendo manejar datos con patrones precisos y complejos.

## Cómo hacerlo:

Vamos a usar `grep`, la herramienta más usada con las expresiones regulares en Bash. Aquí unos ejemplos:

```Bash
# Busca el patrón "Hola" en un archivo
grep "Hola" archivo.txt

# Busca líneas que empiezan por "Hola"
grep "^Hola" archivo.txt

# Busca líneas que terminan en "Adios"
grep "Adios$" archivo.txt
```

La salida será todas las líneas que cumplan con los patrones de búsqueda.

## Excava más fondo

Las expresiones regulares provienen de la teoría matemática de autómatas en los años 50. Su implementación en Bash es típicamente "POSIX básico" o "POSIX extendido". 

Existen opciones alternativas como Perl y Python que ofrecen expresiones regulares más potentes y versátiles. O también puedes usar `awk` y `sed` en Bash.

Recuerda que las expresiones regulares en Bash son de comparación lineal. Esto significa que leen el archivo línea por línea. Si tu archivo es grande, esto puede terminar consumiendo mucho tiempo y memoria.

## Ver también

Aquí algunos recursos adicionales que te podrían ser útiles:

- Más sobre `grep`: https://www.gnu.org/software/grep/manual/grep.html
- Aprende más sobre expresiones regulares: https://www.regular-expressions.info/
- Una útil herramienta para probar tus expresiones regulares: https://regexr.com/  

Y eso es todo, mis amigos programadores. Espero que esta guía desde el mundo del bash os ayude en vuestra aventura con las expresiones regulares!