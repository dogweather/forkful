---
title:                "Fish Shell: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena en el Shell de Fish?

La capitalización de una cadena puede ser útil en diversas situaciones al programar en el Shell de Fish. Por ejemplo, si estás escribiendo un guion para una aplicación o script que requiere que ciertas palabras estén en mayúsculas, capitalizar una cadena puede ahorrar tiempo y esfuerzo en la codificación. Además, puede mejorar la legibilidad del código y hacer que sea más fácil de entender para otros programadores.

## Cómo capitalizar una cadena en el Shell de Fish

Para capitalizar una cadena en el Shell de Fish, puedes usar el comando `string capitalize` seguido de la cadena que deseas capitalizar. Por ejemplo, si tenemos una cadena "hola mundo", podemos capitalizarla utilizando el siguiente código:

```Fish Shell
string capitalize "hola mundo"
```

La salida sería "Hola Mundo", con la primera letra de cada palabra en mayúscula.

Otra forma de capitalizar una cadena en el Shell de Fish es utilizando el comando `string toupper`, que convierte todos los caracteres de una cadena a mayúsculas. Por ejemplo:

```Fish Shell
string toupper "hola mundo"
```

La salida sería "HOLA MUNDO".

Si deseas capitalizar solo la primera letra de una cadena y convertir el resto a minúsculas, puedes utilizar el comando `string capitalize_first`. Por ejemplo:

```Fish Shell
string capitalize_first "hOLA mUNDO"
```

La salida sería "Hola mundo".

## Profundizando en la capitalización de cadenas en Fish Shell

Hay algunas cosas importantes para tener en cuenta al trabajar con la capitalización de cadenas en el Shell de Fish. Primero, es importante recordar que la capitalización distingue entre letras mayúsculas y minúsculas. Por lo tanto, "hola" y "Hola" se consideran cadenas diferentes y serán capitalizadas de manera diferente.

Además, la capitalización de una cadena que ya está en mayúsculas no tendrá efecto. Por ejemplo, si intentamos capitalizar la cadena "HOLA MUNDO", la salida seguirá siendo "HOLA MUNDO".

Otra cosa a tener en cuenta es que el comando `string capitalize` solo capitalizará la primera letra de una cadena si esa letra es una letra minúscula. Si la primera letra ya está en mayúscula, no se realizará ninguna capitalización.

## Ver también
- [Documentación oficial de Fish Shell sobre la manipulación de cadenas](https://fishshell.com/docs/current/cmds/string.html)
- [Tutorial de Shell de Fish en español](https://carlos-jenkins.com/tutoriales/programar-bash-fish-shell-desde-0/)