---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La lectura de argumentos desde la línea de comandos con Bash nos permite pasar información a scripts al momento de ejecución. Hacerlo nos brinda flexibilidad, permitiendo que nuestros programas se adapten a diferentes situaciones.

## ¿Cómo hacerlo?

Aquí hay un ejemplo de cómo leer argumentos de línea de comandos con Bash:

```Bash
#!/bin/bash
echo "Nombre del script: $0"
echo "Primer argumento: $1"
echo "Segundo argumento: $2"
echo "Número de argumentos: $#"
```

Este script imprimirá el nombre del script, los dos primeros argumentos y el número total de argumentos.

Ejecutarlo podría verse así:

```Bash
$ ./mi_script.sh hola mundo
Nombre del script: ./mi_script.sh
Primer argumento: hola
Segundo argumento: mundo
Número de argumentos: 2
```

## Inmersión profunda

Los argumentos de la línea de comandos se han utilizado desde los primeros días de Unix. Son una forma conveniente de pasar información a programas y scripts.

Existen formas alternativas de pasar información a un script de Bash, como leer de un archivo o usar variables de entorno.

Los argumentos de línea de comandos se guardan en las variables especiales `$0`, `$1`, `$2`, etc. La `$0` contiene el nombre del script y los números más altos corresponden a argumentos adicionales en orden.

Si se pasan más de 9 argumentos, deben referenciarse con corchetes: `${10}`, `${11}`, etc.

## Ver también

Para más información sobre argumentos en línea de comandos, visita estes enlaces:

- [Tutorial de Bash Scripting](https://ryanstutorials.net/bash-scripting-tutorial/bash-input.php)
- [Guía de Bash de GNU](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters)