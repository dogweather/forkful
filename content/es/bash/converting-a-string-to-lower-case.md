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
Convertir una cadena de texto a minúsculas es el proceso de cambiar todas las letras de una cadena a su versión en minúsculas. En términos técnicos, esto se conoce como "normalización de mayúsculas y minúsculas". Los programadores a menudo realizan esta tarea para facilitar la comparación y manipulación de cadenas de texto en su código.

## Cómo hacerlo:
Para convertir una cadena de texto a minúsculas en Bash, podemos usar el comando `tr` seguido de las opciones `-s` para eliminar repeticiones y `-d` para eliminar un conjunto de caracteres específico. A continuación se muestra un ejemplo de código y su resultado:
```
cadena="Hola MUNDO"
echo "$cadena" | tr -s '[:upper:]' '[:lower:]'
```
Salida: `hola mundo`

## Inmersión profunda:
Este proceso de normalización de mayúsculas y minúsculas se ha utilizado durante mucho tiempo en la impresión y la tipografía, y se remonta a la época de las máquinas de escribir. En lugar de usar el comando `tr`, también podemos lograr el mismo resultado utilizando la herramienta `sed` o la función incorporada `lower()` en lenguajes de programación como Python y PHP.

## Véase también:
- [Documentación de Bash sobre el comando `tr`](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Documentación de Bash sobre la función `lower()`](https://www.gnu.org/software/bash/manual/html_node/String-Manipulation.html#index-lower-_0028_0029)
- [Documentación de Python sobre la función `lower()`](https://docs.python.org/es/3/library/stdtypes.html#str.lower)