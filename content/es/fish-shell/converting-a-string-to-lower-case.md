---
title:                "Fish Shell: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Con la programación, a menudo es necesario realizar acciones específicas en una cadena de texto, como convertir todas las letras a minúsculas. En este artículo, aprenderás cómo hacerlo en Fish Shell de una manera sencilla.

## Cómo hacerlo

```Fish Shell
set my_string "Hola Mundo"
echo $my_string | tr "A-Z" "a-z"
```

El código anterior asigna la cadena "Hola Mundo" a la variable `my_string` y luego usa el comando `tr` para transformar todas las letras de mayúsculas a minúsculas. El resultado obtenido será "hola mundo".

## Profundizando

Este proceso de convertir una cadena a minúsculas puede variar según el lenguaje de programación utilizado. En simples términos, el comando `tr` busca en la salida del comando `echo` todas las letras mayúsculas y las reemplaza por sus correspondientes minúsculas. Esto es posible gracias al uso de la opción `a-z` en el comando.

Si estás trabajando con una cadena de texto más larga, también puedes utilizar el comando `lowercase`. Por ejemplo:

```Fish Shell
set my_string "Esto Es Un Ejemplo"
lowercase $my_string
```

El resultado sería "esto es un ejemplo". Además, tienes la opción de utilizar la función `string tolower` para transformar una cadena a minúsculas.

## Ver también

Para más información sobre las distintas formas de trabajar con cadenas de texto en Fish Shell, puedes consultar la documentación oficial:
- [Comando `tr`](https://fishshell.com/docs/current/cmds/tr.html)
- [Función `string tolower`](https://fishshell.com/docs/current/commands.html#string-tolower)
- [Comando `lowercase`](https://fishshell.com/docs/current/cmds/lowercase.html)