---
title:                "Leyendo argumentos de la línea de comandos."
html_title:           "Bash: Leyendo argumentos de la línea de comandos."
simple_title:         "Leyendo argumentos de la línea de comandos."
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Leer argumentos de línea de comando en Bash es una forma de permitir que los programas acepten información del usuario al ejecutarse. Los programadores lo hacen para que sus programas puedan ser más interactivos y personalizables.

## Cómo:

Los argumentos de línea de comando se pueden leer en Bash utilizando la variable especial "$@", que contiene una lista de todos los argumentos ingresados al ejecutar un programa. Por ejemplo:

```Bash
#!/bin/bash
echo "El primer argumento es: $1"
echo "El segundo argumento es: $2"
```

Si ejecutamos este programa con los argumentos "hola" y "mundo", el resultado sería:

```Bash
$ bash programa.sh hola mundo
El primer argumento es: hola
El segundo argumento es: mundo
```

## Profundizando:

Los argumentos de línea de comando han estado presentes en los sistemas operativos Unix desde hace décadas, lo que los hace una herramienta fundamental para la interacción con el usuario. Además de leer los argumentos con la variable "$@", también es posible utilizar el comando "getopts" para leer argumentos con opciones y argumentos con valores. Otras alternativas para leer argumentos en Bash incluyen el uso de la función "read" y la implementación de opciones con la librería "getopt".

## Ver también:

- [Documentación de la variable especial "$@"](https://www.gnu.org/software/bash/manual/html_node/Special-Parameters.html)
- [Documentación del comando "getopts"](https://www.gnu.org/software/bash/manual/html_node/Bourne-Shell-Builtins.html#Bourne-Shell-Builtins)
- [Ejemplo de implementación de opciones en Bash](https://github.com/spf13/cobra/blob/master/cmd/cobra/create.go)