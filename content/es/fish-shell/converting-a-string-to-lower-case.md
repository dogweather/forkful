---
title:    "Fish Shell: Convirtiendo una cadena a minúsculas"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

La conversión de una cadena de texto a minúsculas es una tarea común en la programación. En lugar de escribir constantemente código para convertir manualmente cada letra a minúscula, el Fish Shell ofrece una forma rápida y sencilla de hacerlo.

## Cómo

Para convertir una cadena de texto a minúsculas en Fish Shell, simplemente utilizamos el comando ```string tolower``` seguido de la cadena que queremos convertir. Por ejemplo:

```Fish Shell
string tolower "HOLA A TODOS"
```

La salida de este comando sería "hola a todos" en minúsculas.

Otra forma de realizar esta tarea es utilizando el operador ```|```, que permite utilizar el resultado de un comando como entrada para otro. Por ejemplo:

```Fish Shell
echo "HOLA A TODOS" | string tolower
```

La salida de este comando sería la misma que en el ejemplo anterior.

## Deep Dive

Detrás de escena, el comando ```string tolower``` utiliza la función ```lowercase``` del lenguaje de programación interno de Fish Shell, que convierte cada letra en la cadena a su equivalente en minúscula. Además, esta función también es sensible a la localización, lo que significa que tomará en cuenta la configuración regional del sistema para la conversión de ciertos caracteres especiales.

En caso de que necesite convertir una cadena en minúsculas pero mantener algunas letras en mayúsculas, puede utilizar el operador ```[^]``` y especificar los caracteres que desea mantener. Por ejemplo:

```Fish Shell
string tolower "Hola A Todos" | string replace '[^o]' 'O'
```

Este comando reemplaza todas las letras "O" que fueron convertidas a minúsculas por el comando anterior, y las devuelve a mayúsculas.

## Ver también

- [Guía de referencia de Fish Shell](https://fishshell.com/docs/current/cmds/lowercase.html)
- [Cómo convertir texto a minúsculas en Fish Shell](https://fishshell.com/docs/current/cmds/string.html#tolower)