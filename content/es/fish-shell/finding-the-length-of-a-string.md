---
title:                "Fish Shell: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# ¿Por qué encontrar la longitud de una cadena?

En la programación, a menudo nos enfrentamos a situaciones en las que necesitamos conocer la longitud de una cadena de texto. Ya sea para establecer límites, validar entradas o realizar operaciones matemáticas, conocer la longitud de una cadena puede ser una habilidad útil en la programación. En este artículo, aprenderemos cómo encontrar la longitud de una cadena en Fish Shell.

## Cómo hacerlo

Fish Shell nos ofrece una función integrada llamada `string length` que nos permite encontrar la longitud de una cadena. Simplemente escribimos `string length mi_cadena` en la línea de comandos y Fish Shell nos devolverá el número de caracteres que contiene la cadena.

```Fish Shell
string length "Hola mundo"
```

Salida: `10`

También podemos almacenar la longitud de una cadena en una variable para su uso posterior, utilizando el operador de asignación `=`:

```Fish Shell
set mi_longitud (string length "Ejemplo")
echo $mi_longitud
```

Salida: `7`

## Profundizando

Existen varias formas de encontrar la longitud de una cadena en Fish Shell, ya sea utilizando la función `string length`, la función `count` o incluso la función `wc`. Sin embargo, la función `string length` es la más común y recomendada.

Además, es importante tener en cuenta que la longitud de una cadena se mide en caracteres, no en palabras. Por lo tanto, si hay espacios en blanco o caracteres especiales en una cadena, estos también se tendrán en cuenta al calcular la longitud.

## Vea también

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/)
- [Tutorial de Fish Shell en español](https://www.atareao.es/tutorial/fish-shell/)
- [Documentación sobre la función `string length`](https://fishshell.com/docs/3.1/cmds/string-length.html)