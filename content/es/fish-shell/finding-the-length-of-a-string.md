---
title:                "Fish Shell: Encontrando la longitud de una cadena."
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué
En esta guía, vamos a explorar cómo encontrar la longitud (o número de caracteres) de una cadena en Fish Shell. Esto es útil para varias tareas de programación, como validar la entrada de datos o formatear la salida. Además, entender cómo encontrar la longitud de una cadena es un concepto fundamental en la programación y puede ser utilizado en una amplia gama de aplicaciones.

## Cómo hacerlo
En Fish Shell, podemos utilizar el comando `count` para encontrar la longitud de una cadena. Este comando toma la cadena como argumento y devuelve el número de caracteres en esa cadena. Veamos un ejemplo:

```Fish Shell
set str "Hola Mundo!"
echo (count $str)
```

La salida de este código será `11`, ya que la cadena "Hola Mundo!" tiene 11 caracteres en total. También podemos utilizar `count` en cadenas almacenadas en variables, como en el siguiente ejemplo:

```Fish Shell
set fruit "manzana"
set count (count $fruit)
echo "Hay $count caracteres en la palabra $fruit"
```

La salida de este código será "Hay 7 caracteres en la palabra manzana", ya que la cadena "manzana" tiene 7 caracteres.

## Profundizando
En Fish Shell, la función `count` se basa en el comando `string length`. Esto significa que podemos utilizar `string length` directamente en lugar de `count` para lograr el mismo resultado. Además, `string length` acepta más atajos que `count`, como aceptar más de una cadena como argumento. Esto significa que podemos encontrar la longitud de varias cadenas a la vez utilizando `string length`, como se muestra en el siguiente ejemplo:

```Fish Shell
set str1 "Hola"
set str2 "Mundo"
echo (string length $str1 $str2)
```

La salida de este código será `9`, ya que el comando cuenta los caracteres en ambas cadenas.

## Ver también
- Documentación oficial de Fish Shell sobre `count`: https://fishshell.com/docs/current/cmds/count.html
- Documentación oficial de Fish Shell sobre `string length`: https://fishshell.com/docs/current/cmds/string-length.html