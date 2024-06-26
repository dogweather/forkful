---
date: 2024-01-26 03:39:23.166610-07:00
description: "C\xF3mo hacerlo: Fish tiene una magia incorporada para este tipo de\
  \ tarea. Usa la funci\xF3n `string` sin despeinarte. Mira estos hechizos."
lastmod: '2024-03-13T22:44:59.488288-06:00'
model: gpt-4-0125-preview
summary: Fish tiene una magia incorporada para este tipo de tarea.
title: Eliminando comillas de una cadena
weight: 9
---

## Cómo hacerlo:
Fish tiene una magia incorporada para este tipo de tarea. Usa la función `string` sin despeinarte. Mira estos hechizos:

```fish
# Ejemplo con comillas simples
set quoted "'¡Hola, Mundo!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # Salida: ¡Hola, Mundo!

# Lo mismo con comillas dobles
set double_quoted "\"¡Hola, Universo!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # Salida: ¡Hola, Universo!
```

## Análisis Profundo
En la edad de piedra de la línea de comandos, lucharías con `sed` o `awk` para eliminar las comillas; un verdadero enredo de barras invertidas y banderas crípticas. La función `string` de Fish es de una era más nueva, haciendo el código más limpio e intuitivo.

Las alternativas en otros shells podrían depender aún de estas viejas herramientas o podrían usar sus propios métodos incorporados como la expansión de parámetros de bash o los modificadores de zsh.

La función `string` va más allá de recortar comillas. Es una navaja suiza para las operaciones de cadenas en Fish. Con `string`, puedes cortar, dividir, unir, o incluso hacer coincidir con expresiones regulares las cadenas directamente en tu terminal.

## Ver También
Profundiza en `string` con la ayuda de la documentación oficial:
- [Documentación de Cadena de Fish Shell](https://fishshell.com/docs/current/commands.html#string)

Para nostálgicos o cuando se escribe scripts con shells más tradicionales, consulta:
- [Guía de Sed & Awk](https://www.grymoire.com/Unix/Sed.html)
- [Expansión de Parámetros de Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
