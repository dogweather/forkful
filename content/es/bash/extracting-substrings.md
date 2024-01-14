---
title:                "Bash: Extrayendo subcadenas"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas?

Extraer subcadenas es una tarea común en la programación bash. Puede ser útil al manipular cadenas de texto para obtener solo la parte que necesitamos o al realizar búsquedas y reemplazos en texto específico. En esta publicación, aprenderemos cómo extraer subcadenas en bash y profundizaremos en algunos conceptos clave relacionados.

## Cómo hacerlo

Para extraer una subcadena en bash, utilizamos el comando `substring` seguido de la cadena original y los índices de inicio y finalización de la subcadena. Por ejemplo, si queremos extraer solo los primeros tres caracteres de una cadena, podemos usar el siguiente comando:

```bash
substring "hola mundo" 0 3
```

Esto producirá la salida "hol". También podemos utilizar un índice negativo para indicar el número de caracteres desde el final de la cadena. Por ejemplo, si queremos extraer los últimos tres caracteres de la misma cadena, podemos usar:

```bash
substring "hola mundo" -3 0
```

Esto producirá la salida "ndo".

Existen diferentes formas de utilizar el comando `substring` en conjunción con otros comandos, como `grep` o `sed`, para realizar búsquedas y reemplazos en archivos de texto. Puedes encontrar ejemplos y obtener más información sobre la manipulación de subcadenas en bash en [este enlace](https://www.thegeekstuff.com/2010/07/bash-string-manipulation/).

## Profundizando en la extracción de subcadenas

Además de los índices de inicio y finalización, existen otros parámetros que podemos utilizar al extraer subcadenas en bash. Por ejemplo, podemos utilizar el parámetro `-n` para especificar el número de caracteres que queremos extraer, en lugar de los índices de inicio y finalización. También podemos usar el parámetro `-r` para invertir el orden de la subcadena.

Además, bash también ofrece la posibilidad de utilizar expresiones regulares al extraer subcadenas. Esto nos permite ser más precisos en nuestras búsquedas y reemplazos de texto. Puedes obtener más información sobre las expresiones regulares en bash en [este enlace](https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions-in-sed.html).

## Ver también

- [Documentación oficial de bash](https://www.gnu.org/software/bash/documentation/)
- [Tutorial de introducción a bash en español](https://programacion.net/articulo/bash_un_tutorial_de_introduccion_para_principiantes_1446)
- [Ejemplos prácticos de extracción de subcadenas en bash](https://linuxhint.com/extract_substring_bash_script/)