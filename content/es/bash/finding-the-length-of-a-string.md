---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Bash: Cómo encontrar la longitud de una cadena

## ¿Qué & Por Qué?

La longitud de una cadena es el número de caracteres que contiene. Los programadores suelen encontrar la longitud de una cadena para manipular textos, validar entradas o optimizar el rendimiento.

## ¿Cómo se hace?

En Bash, puedes usar la sintaxis ` ${#string} ` para obtener la longitud de una cadena. Aquí está el código de muestra:

```Bash
cadena="Hola Mundo"
longitud=${#cadena}
echo $longitud
```

Este programa imprimirá `10`, que es la longitud de la cadena "Hola Mundo".

## Análisis Profundo

Históricamente, encontrar la longitud de una cadena en Bash ha sido un enfoque común para el procesamiento de texto. Sin embargo, hay alternativas como `expr length "$string"` y `echo -n "$string" | wc -c`, aunque son menos eficientes ya que invocan subprocesos en lugar de usar la operación interna de Bash.

En cuanto a la implementación, cuando Bash evalúa `${#string}`, realiza un recorrido desde el inicio hasta el final de la cadena para contar el número de caracteres. 

## Ver También

Para más información, echa un vistazo a estos recursos:

1. [Guía completa de Bash String Length](https://www.baeldung.com/linux/bash-string-length)
2. [Documentación oficial de Bash](https://www.gnu.org/software/bash/manual/bash.html)
3. [Explicación más detallada de las operaciones de cadena de Bash](https://www.tutorialkart.com/bash-shell-scripting/bash-string/)