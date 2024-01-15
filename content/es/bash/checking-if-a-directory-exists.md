---
title:                "Comprobar si existe un directorio"
html_title:           "Bash: Comprobar si existe un directorio"
simple_title:         "Comprobar si existe un directorio"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

A veces cuando escribimos scripts en Bash, es posible que necesitemos verificar si un directorio existe antes de realizar ciertas acciones. Esto puede ser útil para garantizar que nuestro código se ejecute de manera adecuada y para evitar errores o fallos en nuestro programa.

## Cómo hacerlo

Para comprobar si un directorio existe en Bash, podemos utilizar el comando `test` con la opción `-d` seguido del nombre del directorio que queremos verificar. Un ejemplo sería:

```Bash
if test -d Documents; then
  echo "El directorio Documents existe"
else
  echo "El directorio Documents no existe"
fi
```

El comando `test -d` devuelve un valor booleano, `verdadero` si el directorio existe y `falso` si no existe. De esta manera, podemos usarlo en una declaración `if` para realizar acciones en consecuencia.

## Deep Dive

La opción `-d` del comando `test` verifica si el argumento dado es un directorio. Sin embargo, también existen otras opciones que pueden ser útiles en diferentes situaciones. Algunas de ellas son:

- `-e` verifica si el argumento existe, ya sea un archivo, directorio, o cualquier otro elemento en el sistema.
- `-f` verifica si el argumento existe y es un archivo regular (no un enlace simbólico o un directorio).
- `-r` verifica si el argumento es un archivo y tiene permiso de lectura.
- `-w` verifica si el argumento es un archivo y tiene permiso de escritura.
- `-x` verifica si el argumento es un archivo y tiene permiso de ejecución.

Para más información sobre las diferentes opciones del comando `test`, puedes consultar su manual utilizando el comando `man test` en tu terminal.

## Ver también

- [Manual del comando test](https://www.linuxcommand.org/lc3_man_pages/testh.html)
- [Comandos de Linux útiles para scripting en Bash](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Condiciones y bucles en scripts de Bash](https://www.shellscript.sh/flow.html)