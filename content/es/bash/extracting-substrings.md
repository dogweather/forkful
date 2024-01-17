---
title:                "Extrayendo subcadenas"
html_title:           "Bash: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Extraer subcadenas es una técnica común en la programación que consiste en obtener una porción específica de una cadena de texto. Los programadores hacen esto para manipular y analizar datos en una cadena de texto de manera más eficiente.

## Cómo hacerlo:

Para extraer subcadenas en Bash, puedes utilizar el comando `cut`. A continuación se muestra un ejemplo de cómo extraer los primeros 5 caracteres de una cadena:

```Bash
cadena="Hola Mundo"
echo ${cadena:0:5}
```

La salida de este código sería `Hola `. De manera similar, puedes extraer desde un punto específico hasta el final de la cadena utilizando un número negativo en el segundo parámetro:

```Bash
cadena="Hola Mundo"
echo ${cadena:5}
```

La salida sería `Mundo`.

## Buceo profundo:

Extraer subcadenas ha sido una técnica utilizada desde los primeros días de la programación. Antes de Bash, había otros lenguajes de programación que también tenían funciones para extraer subcadenas, como Perl o C. También hay alternativas en Bash, como los comandos `grep` y `sed`, que pueden ser utilizados para extraer subcadenas de manera más compleja.

## Véase también:

Si deseas aprender más sobre la manipulación de cadenas de texto en Bash, puedes consultar la documentación oficial de Bash en línea. También puedes encontrar ejemplos y tutoriales en línea que pueden ayudarte a comprender mejor cómo extraer subcadenas en Bash.