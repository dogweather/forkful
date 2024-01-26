---
title:                "Comprobando si existe un directorio"
date:                  2024-01-20T14:56:08.277577-07:00
html_title:           "Gleam: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Comprobar si un directorio existe en Fish Shell te permite asegurar que puedas leer o escribir en dicho directorio sin problemas. Los programadores hacen esto para manejar errores y evitar sorpresas desagradables como intentar acceder a algo que no está ahí.

## Cómo Hacerlo:
```Fish Shell
if test -d /ruta/al/directorio
    echo "El directorio existe."
else
    echo "El directorio no existe."
end
```

Ejemplo de salida si el directorio existe:
```
El directorio existe.
```

Ejemplo de salida si el directorio no existe:
```
El directorio no existe.
```

## Inmersión Profunda
En Fish Shell, el comando `test` permite realizar pruebas de archivos y directorios. El flag `-d` se usa específicamente para directorios. Antiguamente, en otros shells como Bash, se utilizaba `[ -d /ruta ] && echo "Existe"` lo que también se puede hacer en Fish, pero `test` es más legible y preferido.

Otras alternativas incluyen usar `stat` y capturar errores, o incluso intentar cambiar al directorio con `cd` y verificar si fue exitoso. Estas alternativas pueden ofrecer más información, como los permisos del directorio.

En cuanto a la implementación, Fish ejecuta `test` como un comando integrado.
Esto significa que su ejecución es rápida y eficiente ya que no se inicia un proceso externo.

## Véase También
- Documentación del comando `test`: https://fishshell.com/docs/current/commands.html#test
- Tutorial oficial de Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Guía de scripting en Fish: https://fishshell.com/docs/current/index.html#scripting
