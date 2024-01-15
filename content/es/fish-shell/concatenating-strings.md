---
title:                "Uniendo cadenas de texto"
html_title:           "Fish Shell: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué utilizar concatenación de cadenas en Fish Shell?

Si alguna vez has necesitado combinar dos o más cadenas en un programa de shell, entonces la concatenación de cadenas en Fish Shell te puede ser de gran ayuda. Esta función te permite unir diferentes cadenas de texto para crear una cadena más larga y manipularla según sea necesario en tus scripts.

## Cómo realizar concatenación de cadenas en Fish Shell

La sintaxis para concatenar cadenas en Fish Shell es bastante simple. Utiliza el operador `+` para unir dos cadenas juntas. Aquí hay un ejemplo de cómo unir las cadenas "Hola" y "mundo" en una sola cadena:

```Fish Shell
set saludo "Hola"
set nombre "mundo"
echo $saludo + $nombre
```

La salida de este código será "Hola mundo". También puedes concatenar más de dos cadenas a la vez utilizando el operador `+` varias veces.

## Profundizando en la concatenación de cadenas

Además del operador `+`, Fish Shell también ofrece otras formas de concatenar cadenas. Puedes utilizar el comando `string` seguido de la opción `concat` para unir cadenas. También puedes utilizar el comando `string` con la opción `join` para unir cadenas con un separador específico entre ellas.

Otra forma de concatenar cadenas es utilizando el comando `string` con la opción `sub`.

Incluso puedes utilizar el comando `string` con la opción `find` para buscar y concatenar cadenas que cumplan con ciertas condiciones.

## También te puede interesar

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guía de instalación y configuración de Fish Shell](https://blog.jonathan.camp/managing-dotfiles-with-onefetch)
- [Ejemplos de scripts en Fish Shell](https://gist.github.com/adi1090x/372905a6466d7c1b1eeb5075fc198ba1)