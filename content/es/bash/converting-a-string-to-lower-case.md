---
title:                "Convirtiendo una cadena a minúsculas."
html_title:           "Bash: Convirtiendo una cadena a minúsculas."
simple_title:         "Convirtiendo una cadena a minúsculas."
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Converting a string to lower case is a common task in Bash programming. It allows you to manipulate text in a case-insensitive manner, making it easier to search, compare, and manipulate strings.

## ¿Cómo hacerlo?

La conversión de una cadena a minúsculas en Bash es muy sencilla. Puedes utilizar el comando `tr` o la utilidad de sustitución de Bash (`${VAR,,}`). Ambos métodos funcionan de manera similar, pero el comando `tr` es más versátil y funciona con una amplia gama de caracteres.

```
# Utilizando el comando 'tr'
cadena="MUNDO EXITOSO"

echo $cadena | tr '[:upper:]' '[:lower:]' # salida: mundo exitoso

# Utilizando la utilidad de sustitución de Bash
echo ${cadena,,} # salida: mundo exitoso
```

Ambos métodos convierten la cadena en minúsculas y retornan el resultado en un nuevo valor, sin modificar la cadena original. También puedes utilizar las opciones `-t` e `-d` para manejar caracteres especiales o eliminarlos en la conversión.

## Profundizando

El comando `tr` funciona utilizando caracteres o patrones para indicar qué caracteres deben ser modificados en la cadena de entrada y cómo deben ser modificados. Puedes consultar la documentación para obtener una lista completa de opciones y cómo utilizarlas.

La utilidad de sustitución de Bash, por otro lado, utiliza un patrón glob para indicar qué parte de la cadena debe ser modificada. Este patrón glob es muy similar a las expresiones regulares, pero con algunas diferencias clave. Puedes utilizar la opción `-C` para obtener más información sobre cómo funciona el patrón glob en la utilidad de sustitución.

## Ver también

- Documentación de `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Documentación de la utilidad de sustitución de Bash: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion