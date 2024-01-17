---
title:                "Comprobando si existe un directorio"
html_title:           "Fish Shell: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

¡Hola amigos programadores! En este artículo, vamos a hablar sobre cómo checar si un directorio existe en el lenguaje de programación Fish Shell.

## ¿Qué y por qué?
Revisar si un directorio existe es simplemente verificar si una ubicación en tu computadora es realmente un directorio. Los programadores suelen hacer esto como parte de sus scripts o programas para asegurarse de que están accediendo a la ubicación correcta antes de realizar cualquier acción.

## ¿Cómo hacerlo?
Usando el comando `test` y la opción `-d` en la terminal de Fish Shell, podemos verificar si un directorio existe.

```
test -d /ruta/a/mi_directorio
```

Si el directorio existe, el comando no devolverá ningún mensaje. Pero si no existe, devolverá un error indicando que el directorio no fue encontrado.

```
test: /ruta/a/mi_directorio: No such file or directory
```

## Inmersión profunda
Checar si un directorio existe puede ser útil cuando estás escribiendo scripts que involucren la creación o el acceso a directorios específicos. Antes de Fish Shell, otros shells como Bash y Zsh también contaban con este comando.

Como alternativa, también existe el comando `dirh`, que devuelve un error si el directorio no existe en lugar de no mostrar nada. También puedes usar el comando `test` con la opción `-e` para verificar si un archivo o directorio existe en general.

## Ver también
Para más información sobre los comandos `test` y `dirh`, puedes consultar la documentación oficial de Fish Shell. También puedes revisar ejemplos de scripts en línea que utilizan estos comandos para checar la existencia de directorios. ¡Hasta la próxima!