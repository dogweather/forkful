---
title:                "Usando expresiones regulares"
html_title:           "Go: Usando expresiones regulares"
simple_title:         "Usando expresiones regulares"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Las expresiones regulares son una forma de buscar y manipular el texto en función de patrones específicos. En la programación, se utilizan para tareas como la validación de entradas y el análisis de archivo de Logs.

## Cómo se hace
Usar expresiones regulares en Fish es relativamente sencillo. Aquí tienes un ejemplo de cómo detectar si un texto tiene caracteres alfanuméricos:

```fish
set texto "Hola Mundo123"
if echo "$texto" | string match -r -q '([A-Za-z0-9]+)'
  echo "El texto contiene caracteres alfanuméricos"
end
```
El resultado sería:
```fish
El texto contiene caracteres alfanuméricos
```

## Inmersión profunda
Las expresiones regulares tienen su origen en la teoría de lenguajes formales en matemáticas y en la informática teórica. En Fish, utilizamos la sintaxis extendida de las expresiones regulares POSIX. Como alternativa, podrías usar lenguajes de programación como Perl y Python, que también admiten expresiones regulares.

La implementación en Fish es particularmente útil debido a su enfoque en la interactividad y la eficacia del script, así como a su interoperabilidad con otros lenguajes y herramientas.

## Ver también
Una amplia variedad de fuentes están disponibles para seguir aprendiendo y experimentando con las expresiones regulares en Fish. Aquí tienes algunos enlaces:

- Documentación oficial de Fish: https://fishshell.com/docs/current/index.html
- Tutorial de Expresiones Regulares: https://www.regular-expressions.info/tutorial.html
- POSIX Regex Syntax: https://www.gnu.org/software/libc/manual/html_node/POSIX-Regexps.html