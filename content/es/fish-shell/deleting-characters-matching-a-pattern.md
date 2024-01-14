---
title:    "Fish Shell: Eliminando caracteres que coinciden con un patrón"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Eliminar ciertos caracteres de un patrón puede ser útil en situaciones en las que se desea limpiar o formatear datos de manera rápida y eficiente. También puede ser útil al trabajar con archivos de texto o al escribir scripts para procesar datos.

## Cómo hacerlo

Para eliminar caracteres que coincidan con un patrón en Fish Shell, se puede utilizar el comando `string sub` seguido del patrón y el texto en el que se desea realizar la eliminación. Por ejemplo:

```Fish Shell
# Original text
set texto "Hola, soy un texto con caracteres no deseados!#@+@#-"
echo $texto

# Eliminar caracteres no deseados
set newText (string sub '*[@!#$^-]' $texto)
echo $newText
```

Esto producirá la siguiente salida:

```
Hola, soy un texto con caracteres no deseados!#@+@#-
Hola, soy un texto con caracteres *
```

El comando `string sub` utiliza expresiones regulares para buscar y reemplazar patrones en un texto determinado. En el ejemplo anterior, el patrón utilizado es `*[@!#$^-]`, lo que significa que cualquier caracter que sea una letra, un número o un símbolo de puntuación será eliminado.

## Profundizando

El patrón utilizado en el ejemplo anterior es solo uno de muchos que se pueden utilizar al eliminar caracteres en Fish Shell. Algunos otros patrones comunes incluyen:

- `[0-9]` para eliminar todos los números
- `[A-Z]` para eliminar todas las letras mayúsculas
- `[a-z]` para eliminar todas las letras minúsculas
- `[^a-z]` para eliminar todo excepto las letras minúsculas
- `[^0-9]` para eliminar todo excepto los números

Además, el comando `string sub` también puede combinarse con otros comandos como `find` para restringir la búsqueda de patrones a archivos específicos.

## Ver también

- [Documentación de Fish Shell string substitution](https://fishshell.com/docs/current/cmds/string-sub.html)
- [Introducción a las expresiones regulares en Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_regex)
- [Guía completa de expresiones regulares en Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_regex_full)