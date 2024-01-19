---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

La interpolación de cadenas en Bash es el acto de sustituir un marcador de posición por su valor en una cadena. Los programadores lo hacen para construir cadenas de forma más eficaz y flexible.

## Cómo hacerlo:

Aquí una muestra de cómo se hace la interpolación de cadenas en Bash.

```Bash
nombre="Mundo"
echo "Hola, ${nombre}!"
```

La salida esperada será:

```Bash
Hola, Mundo!
```

## Inmersión Profunda:

El mecanismo de interpolación de cadenas en Bash se originó en los lenguajes de programación de alto nivel, y luego se adoptó en los scripts de shell para permitir una manipulación más avanzada y flexible de las cadenas de texto.

Una alternativa a la interpolación de cadenas sería la concatenación, pero esto puede ser más complicado y menos legible cuando se trata de cadenas más largas o de múltiples variables.

La implementación de la interpolación de cadenas en Bash es bastante directa: cualquier cosa entre ${} es interpretada como una variable y se sustituye su valor en la cadena. Algunos caracteres especiales como '\n' para nueva línea o '\t' para tabulación también pueden ser interpolados en una cadena.

## Ver También:

1. [Guía de Bash](https://www.gnu.org/software/bash/manual/bash.html)
2. [Interpolación de cadenas en Bash](https://www.linuxtopia.org/online_books/advanced_bash_scripting_guide/string-manipulation.html)
3. [Tutorial de Bash para principiantes](https://www.shellscript.sh/)
4. ["Bash for Beginners Guide"](https://tldp.org/LDP/Bash-Beginners-Guide/html/chap_03.html)

Este artículo no concluye aquí. ¡No dejes de aprender, seguir explorando y practicando!