---
title:                "Fish Shell: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué verificar si existe un directorio?

Verificar si un directorio existe es una tarea común al trabajar con la terminal y en programación. Es importante asegurarse de que un directorio exista antes de intentar manipularlo o acceder a él en caso de que no exista. Esto nos ayuda a evitar errores y a escribir código más robusto.

## Cómo hacerlo

En Fish Shell, podemos usar el comando `test -d` seguido del nombre del directorio para verificar si existe o no. Este comando devuelve un valor de exito si el directorio existe y un error si no existe. Por ejemplo:

```Fish Shell
test -d mi_directorio
echo $status
```

Si el directorio "mi_directorio" existe, este comando imprimirá un `0` como resultado. De lo contrario, imprimirá un `1` indicando que no existe.

## Profundizando en la verificación de directorios

Además de usar el comando `test -d`, existen otras formas de verificar si un directorio existe en Fish Shell, como por ejemplo usando el comando `ls`. Este comando nos permite listar el contenido de un directorio específico y si el directorio que intentamos listar no existe, se mostrará un error.

Otra opción es usar el comando `mkdir -p`. Este comando nos permite crear un directorio siempre y cuando no exista previamente. Podemos usarlo para crear un directorio si este no existe y luego verificar su existencia usando `test -d`.

En resumen, es esencial verificar si un directorio existe antes de intentar acceder o manipularlo en Fish Shell. Esto nos ayuda a escribir un código más confiable y a evitar errores.

## Ver también

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/cmds/test.html)
- [Ejemplos de verificación de directorios en Fish Shell](https://www.unixtutorial.org/fish-test-command-synatx/)
- [Otras formas de verificar la existencia de directorios](https://stackoverflow.com/questions/59838/check-if-a-directory-exists-in-a-shell-script)