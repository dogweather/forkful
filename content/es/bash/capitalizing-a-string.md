---
title:                "Capitalizando una cadena"
html_title:           "Bash: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena de texto?

En Bash, a veces es necesario convertir una cadena de texto en mayúsculas, ya sea por estética o por cuestiones técnicas. Al capitalizar una cadena, se asegura que todos los caracteres sean mostrados en su forma en mayúscula, proporcionando uniformidad y claridad en la visualización.

## ¿Cómo hacerlo?

Para capitalizar una cadena en Bash, podemos utilizar el comando `tr`, que se encarga de reemplazar o eliminar caracteres en un texto. Podemos especificar que los caracteres a ser reemplazados sean convertidos a mayúsculas utilizando la opción `-u`. A continuación, un ejemplo de código y su respectiva salida:

```Bash
# Definimos una cadena de texto
texto="hola mundo!"

# Utilizamos el comando tr para convertir la cadena en mayúsculas
echo "$texto" | tr '[:lower:]' '[:upper:]'
```

Salida:
```Bash
HOLA MUNDO!
```

También podemos utilizar la función `declare` para capitalizar una sola palabra en una variable:

```Bash
# Definimos una variable con una palabra en minúscula
palabra="programación"

# Usamos la función declare para convertir la primera letra en mayúscula
declare -u primera_letra="$palabra"

# Imprimimos la variable resultante
echo "$primera_letra"
```

Salida:
```Bash
PROGRAMACIÓN
```

## Un poco más sobre capitalizar una cadena

En el ejemplo anterior, utilizamos la opción `-u` del comando `tr` para convertir una cadena completa en mayúsculas. Sin embargo, existe la opción `-c` que nos permite capitalizar solo la primera letra de cada palabra en una cadena. También se puede combinar el uso de `tr` con otros comandos, como `sed` y `awk`, para obtener resultados más específicos.

## Vea también

- [Documentación oficial de Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Tutorial de Bash en español](https://www.tutorialspoint.com/es/advanced_bash_scripting.htm)
- [Guía rápida de Bash](https://github.com/skyweb-site/awesome-command-line-tools#a0-bash-cli-tool)