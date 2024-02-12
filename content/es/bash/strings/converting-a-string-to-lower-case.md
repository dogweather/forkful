---
title:                "Conversión de una cadena de texto a minúsculas"
aliases:
- /es/bash/converting-a-string-to-lower-case/
date:                  2024-01-20T17:37:56.477225-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversión de una cadena de texto a minúsculas"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Convertir una cadena a minúsculas es el proceso de cambiar todos los caracteres alfabéticos de una cadena de texto para que estén en su forma minúscula. Los programadores hacen esto para uniformizar los datos, facilitar comparaciones insensibles a mayúsculas, o simplemente para cumplir con requisitos de formato específicos.

## Cómo hacerlo:

Aquí tienes ejemplos de cómo convertir texto a minúsculas en bash:

```Bash
# Utilizando tr
echo "HOLA MUNDO" | tr '[:upper:]' '[:lower:]'
```

Salida esperada:
```
hola mundo
```

```Bash
# Utilizando la expansión de parámetros de Bash
texto="HOLA MUNDO"
echo "${texto,,}"
```

Salida esperada:
```
hola mundo
```

## Profundizando

La necesidad de convertir texto a minúsculas es tan antigua como la propia informática. Originalmente, los comandos UNIX, como `tr` o `awk`, fueron utilizados para transformar el texto. Pero Bash, desde la versión 4.0, incluye funcionalidades nativas para manipular cadenas, como la expansión de parámetros que te mostré antes.

Alternativas incluyen el uso de lenguajes de programación más potentes como Python, Perl o Awk para tareas más complejas. Pero para un script rápido y ligero, Bash es más que suficiente.

Detalles de implementación: La expansión de parámetros `${texto,,}` es específica de Bash y no funcionará en otros shells como sh o dash. Además, es importante recordar que esto solo afecta a caracteres ASCII, de modo que si estás trabajando con otros alfabetos o con casos especiales como la letra ñ o caracteres acentuados, podrías necesitar herramientas adicionales o configuraciones de localización (locale).

## Ver También

- Manual de Bash para expansiones de parámetros: [Bash Parameter Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- Tutorial de `tr`: [GNU tr manual](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)

Y si quieres profundizar en el trabajo con caracteres no ASCII:

- Información sobre locales en Unix: [Locale - Unix](https://man7.org/linux/man-pages/man7/locale.7.html)
