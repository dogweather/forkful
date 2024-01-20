---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Convertir una cadena a minúsculas en Bash implica cambiar todos los caracteres alfabéticos en ella a su forma en minúsculas. Los programadores lo hacen para normalizar las entradas de texto y realizar comparaciones no sensibles a mayúsculas y minúsculas.

## Cómo hacerlo:

Aquí está cómo puedes convertir una cadena a minúsculas en Bash:

```bash
cadena="Hola, Mundo"
cadena_minusculas=${cadena,,}
echo $cadena_minusculas
```

El código anterior imprimirá:

```bash
hola, mundo
```

El código utiliza la expansión de parámetros shell de Bash, `${cadena,,}`, para convertir todos los caracteres de `cadena` a minúsculas.

## Profundización

Bash 4.0 introdujo la funcionalidad de expansión de parámetros que también incluye la conversión de minusculas. Antes de esta versión, los programadores tenían que usar otras soluciones menos limpias y eficientes, como `tr '[:upper:]' '[:lower:]'`.

Una alternativa a la expansión de parámetros es usar la función `tolower` de AWK:

```bash
cadena="Hola, Mundo"
cadena_minusculas=$(echo $cadena | awk '{print tolower($0)}')
echo $cadena_minusculas
```

Pero esta opción puede resultar demasiado verbosa y menos eficiente para la mayoría de las tareas.

## Ver también

- Guía del Manual de Bash sobre Expansión de Parámetros: http://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- Manual de AWK para `tolower`: https://www.gnu.org/software/gawk/manual/html_node/String-Functions.html
- Herramienta en línea para aprender Bash: http://www.learnshell.org/