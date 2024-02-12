---
title:                "Capitalizando una cadena de texto"
aliases:
- /es/bash/capitalizing-a-string/
date:                  2024-02-03T19:04:49.084996-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizando una cadena de texto"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y por qué?
Capitalizar una cadena en Bash implica transformar el primer carácter de la cadena a mayúscula mientras que el resto de la cadena permanece sin cambios. Esta técnica se utiliza comúnmente para formatear la salida o cumplir con convenciones de codificación que requieren que ciertas cadenas comiencen con una letra mayúscula por legibilidad o preferencias estilísticas.

## Cómo hacerlo:

Bash no tiene una función integrada específicamente para capitalizar cadenas, pero puedes lograr esta tarea usando expansión de parámetros o herramientas externas como `awk`. Aquí hay algunas formas de capitalizar una cadena en Bash:

**Usando expansión de parámetros:**

Este método manipula la cadena directamente en la shell.

```bash
str="hola mundo"
capitalizado="${str^}"
echo "$capitalizado"
```
Salida:
```
Hola mundo
```

**Usando `awk`:**

`awk` es una poderosa herramienta de procesamiento de texto disponible en la mayoría de los sistemas operativos tipo Unix, que se puede utilizar para capitalizar cadenas.

```bash
str="hola mundo"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
Salida:
```
Hola mundo
```

**Usando `sed`:**

Para un enfoque más tradicional, se puede emplear `sed` para capitalizar la primera letra de una cadena. Sin embargo, es un poco más complejo en comparación con los métodos anteriores.

```bash
str="hola mundo"
echo "$str" | sed 's/./\u&/'
```
Salida:
```
Hola mundo
```

Estos fragmentos demuestran cómo capitalizar la primera letra de una cadena en Bash, destacando la flexibilidad de la programación en shell cuando se manipula texto.
