---
title:                "Eliminando caracteres que coinciden con un patrón."
html_title:           "Fish Shell: Eliminando caracteres que coinciden con un patrón."
simple_title:         "Eliminando caracteres que coinciden con un patrón."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

En ocasiones, es posible que necesitemos eliminar ciertos caracteres de un archivo o texto que coincidan con un patrón específico. Esto puede ser útil para limpiar datos o para realizar cambios en un conjunto de archivos de manera eficiente.

## Cómo hacerlo

Para eliminar caracteres que coincidan con un patrón en Fish Shell, podemos utilizar el comando `string replace`. Este comando acepta tres argumentos: el patrón a buscar, el texto a reemplazar y el texto original donde se realizará el reemplazo.

```Fish Shell
set texto_original "Hola Mundo"
string replace "o" "" $texto_original
```

La salida de este comando sería "Hl Mund", ya que elimina todas las letras "o" del texto original. También podemos utilizar expresiones regulares para buscar patrones más complejos. Por ejemplo, para eliminar todas las vocales de un texto, podríamos usar la expresión regular `"[aeiou]"` como patrón.

```Fish Shell
set texto_original "Este es un texto de ejemplo"
string replace "[aeiou]" "" $texto_original
```

La salida sería "st s n txt d mpl", ya que se eliminaron todas las vocales del texto original.

## Profundizando

El comando `string replace` también tiene algunas opciones que pueden ser útiles en ciertas situaciones. Por ejemplo, podemos utilizar la opción `-r` para realizar el reemplazo de manera recursiva en un directorio completo.

Otra opción útil es la `-s`, que permite reemplazar solo la primera aparición del patrón en cada línea. Esto podría ser útil si solo queremos hacer un cambio específico en un texto sin afectar otras instancias del mismo patrón.

```Fish Shell
set texto_original "123 456 789"
string replace " " "," -s $texto_original
```

La salida sería "123,456 789" ya que solo se reemplazó el primer espacio en blanco con una coma.

## Ver también

- Documentación oficial del comando `string replace`: https://fishshell.com/docs/current/cmds/string-replace.html
- Tutorial de expresiones regulares en Fish Shell: https://fishshell.com/docs/current/tutorial.html#tutorial-regexes
- Ejemplos de uso del comando `string replace`: https://fishshell.com/docs/current/tutorial.html#string-manipulation-options