---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizar un string significa convertir sus letras en mayúsculas. Programadores lo hacen para normalizar datos, mejorar legibilidad, o cumplir con requisitos técnicos.

## How to:
En Bash, capitalizar un texto es sencillo. Usa `tr`, `awk`, o directamente en una variable Bash. Ejemplos:

```Bash
# Usando tr
echo "hola mundo" | tr '[:lower:]' '[:upper:]'
# Salida: HOLA MUNDO

# Usando awk
echo "hola mundo" | awk '{ print toupper($0) }'
# Salida: HOLA MUNDO

# Bash 4.0 o superior: usando variable con expansion
texto="hola mundo"
echo "${texto^^}"
# Salida: HOLA MUNDO
```

## Deep Dive
Antes de Bash 4.0, tenías que confiar en herramientas externas como `tr` o `awk` para cambiar la capitalización. Con Bash 4.0 en adelante, puedes hacerlo dentro del propio lenguaje, lo que puede ser más eficiente. Alternativas como `sed` o Perl también son comunes, pero `awk` y `tr` son típicamente fáciles de recordar y usar para esta tarea.

En cuanto a implementación, `tr` realiza el cambio de caracteres en un flujo de datos (pipe), mientras que las expansiones de variable de Bash hacen el cambio en memoria, lo que es más rápido. Por último, no olvides que la capitalización es dependiente del locale: hacerlo en inglés es diferente que en turco, por ejemplo, donde la 'i' minúscula tiene más de una forma mayúscula.

## See Also
- GNU Bash manual: [https://www.gnu.org/software/bash/manual/](https://www.gnu.org/software/bash/manual/)
- `man tr`: [https://linux.die.net/man/1/tr](https://linux.die.net/man/1/tr)
- `man awk`: [https://linux.die.net/man/1/awk](https://linux.die.net/man/1/awk)
