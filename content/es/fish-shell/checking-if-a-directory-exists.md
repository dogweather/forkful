---
title:                "Comprobando si un directorio existe"
html_title:           "Fish Shell: Comprobando si un directorio existe"
simple_title:         "Comprobando si un directorio existe"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Verificar si un directorio existe es el acto de comprobar si cierta ubicación en el sistema de archivos está presente o no. Los programadores lo hacen para prevenir errores al leer o escribir en un directorio que puede o no estar allí.

## Cómo hacerlo:
Ejemplos de código y resultados en bloques de código `Fish Shell` :
```Fish Shell
# Comprobar si el directorio existe
if test -d /ruta/directorio
    echo "El directorio existe"
else
    echo "El directorio no existe"
end
```
Salida de muestra:
```Fish Shell
El directorio existe
```
O
```Fish Shell
El directorio no existe
```
## Inmersión Profunda:
(1) Contexto histórico: La verificación de directorios ha sido un proceso común en la programación desde los primeros sistemas operativos.
(2) Alternativas: En Fish Shell, se puede usar `test -e /ruta/directorio` para comprobar si un archivo o directorio existe.
(3) Detalles de implementación: `test -d /ruta/directorio` devuelve un valor booleano (`True` o `False`) basado en la presencia del directorio.

## Ver También:
Puede ubicar más detalles sobre el comando `test` y otras utilidades de Fish Shell en su documentación oficial:
[Documentación oficial de Fish Shell](https://fishshell.com/docs/current/commands.html#test)