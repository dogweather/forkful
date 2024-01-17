---
title:                "Comprobando si un directorio existe"
html_title:           "Bash: Comprobando si un directorio existe"
simple_title:         "Comprobando si un directorio existe"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Chequear si un directorio existe es una tarea común en programación que consiste en verificar si un directorio (o carpeta) específico existe en una ubicación determinada. Los programadores lo hacen para asegurarse de que el código no fallará al intentar acceder a un directorio que no existe.

## Cómo hacerlo:

```Bash
if [ -d /ruta/directorio ]; then
  echo "El directorio existe"
else
  echo "El directorio no existe"
fi
```
Salida si el directorio existe:
```
El directorio existe
```
Salida si el directorio no existe:
```
El directorio no existe
```

## Profundizando:

- Contexto histórico: La función para verificar si un directorio existe se introdujo en Bash en la versión 2.0 en 1996.
- Alternativas: Además del método usado en el ejemplo anterior, también se puede utilizar el comando `test -d /ruta/directorio`. También hay herramientas como `stat` y `ls` que permiten verificar si un directorio existe.
- Detalles de implementación: Al utilizar `[ -d /ruta/directorio ]`, se está usando una prueba de condición que devuelve verdadero si el directorio existe y falso si no existe. Si se desea usar una estructura `if` más detallada, también se puede hacer con la sintaxis `if [ -d /ruta/directorio ]; then`.

## Véase también:

- [El comando `test` en Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [Otras herramientas para verificar si un directorio existe](https://www.linuxjournal.com/content/bash-tips-checking-file-or-directory-existence)