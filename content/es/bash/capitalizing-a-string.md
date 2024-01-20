---
title:                "Capitalizando una cadena de texto"
html_title:           "Bash: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Capitalizar una cadena de texto es el proceso de transformar todas las letras minúsculas de la misma en mayúsculas. Los programadores suelen hacerlo para estandarizar su salida de texto o para comparar cadenas sin tener en cuenta si son minúsculas o mayúsculas.

## ¿Cómo hacerlo?

Para capitalizar una cadena en Bash, puedes usar el comando `tr`. Aquí te muestra cómo:

```Bash
echo 'hola mundo' | tr '[:lower:]' '[:upper:]'
```

El resultado será:

```Bash
HOLA MUNDO
```

El comando `tr` se usa para transformar caracteres. Aquí, transforma todos los caracteres de minúsculas a mayúsculas.

## Análisis Profundo

La historia de capitalizar cadenas es bastante simple: se ha hecho desde que se inventaron las primeras computadoras. En Bash, la manera más común de hacerlo es con el comando `tr`, pero hay otras formas de hacerlo. Por ejemplo, podrías usar `awk`, `sed` o incluso `perl` con un one-liner:

```Bash
echo 'hola mundo' | awk '{print toupper($0)}'
echo 'hola mundo' | sed -e 'y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/'
echo 'hola mundo' | perl -ne 'print uc'
```

Todas estas líneas de código capitalizarán el string 'hola mundo'. Sin embargo, `tr` es el comando más común por su simplicidad.

## Ver También

* [Guía de Bash](https://tldp.org/LDP/abs/html/abs-guide.html)
* [AWK - Manual](https://www.gnu.org/software/gawk/manual/gawk.html)
* [SED - Manual](https://www.gnu.org/software/sed/manual/sed.html)
* [Perl - Manual](https://perldoc.perl.org/index-language.html)

Recuerda, también puedes visitar las páginas de manuales de `awk`, `sed`, `perl` y `tr` en tu computadora para aprender más sobre esas herramientas en Bash.