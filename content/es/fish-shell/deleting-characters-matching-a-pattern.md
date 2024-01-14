---
title:                "Fish Shell: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías borrar caracteres que coincidan con un patrón?

A veces, mientras trabajas en tu terminal con Fish Shell, puedes encontrarte con la necesidad de borrar ciertos caracteres de un texto que coinciden con un patrón específico. Esto puede ser útil para limpiar tus archivos de configuración, depurar tu código o simplemente para mantener tu sistema organizado. En esta entrada, te mostraré cómo hacerlo de manera sencilla y eficiente en Fish Shell.

## Cómo hacerlo

Para borrar caracteres que coincidan con un patrón en Fish Shell, utilizaremos el comando `string replace` seguido del patrón que deseamos eliminar y reemplazándolo con un texto vacío. Por ejemplo, si queremos eliminar todos los símbolos de exclamación en un texto, podemos escribir lo siguiente en nuestro terminal:

```Fish Shell

echo "¡Hola mundo!" | string replace "!" ""

```

Esto imprimirá en la línea de comandos el texto "¡Hola mundo" sin el símbolo de exclamación.

## Profundizando

El comando `string replace` en Fish Shell también nos permite usar patrones más complejos utilizando expresiones regulares. Por ejemplo, si deseamos eliminar todos los números de un texto, podemos usar la expresión regular `"[0-9]"` para indicar que queremos borrar cualquier dígito del 0 al 9. El comando se vería así:

```Fish Shell
echo "Hoy es 1 de enero del 2020" | string replace "[0-9]" ""
```

Esto nos dará como resultado el texto "Hoy es de enero del ", sin los números de la fecha.

También podemos usar la opción `-g` para borrar todas las ocurrencias del patrón en lugar de solo la primera. Por ejemplo:

```Fish Shell
echo "¡¡¡Feliz cumpleaños!!!" | string replace "!" "" -g
```

Esto nos devolverá el texto "¡Feliz cumpleaños!" sin los símbolos de exclamación.

## Ver también

Para más información sobre el comando `string replace` en Fish Shell y todos sus parámetros, puedes consultar la [documentación oficial](https://fishshell.com/docs/current/cmds/string-replace.html). También puedes aprender más sobre expresiones regulares en Fish Shell en esta [entrada de blog](https://fishshell.com/docs/current/tutorial.html#using-regular-expressions). ¡Espero que te haya sido útil esta entrada y que te ayude a mejorar tus habilidades en Fish Shell!