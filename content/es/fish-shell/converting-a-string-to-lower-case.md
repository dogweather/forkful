---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Fish Shell: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Convertir una cadena de texto a minúsculas es simplemente cambiar todas las letras mayúsculas a su equivalente en minúsculas. Los programadores a menudo lo hacen para facilitar la comparación de cadenas de texto o para estandarizar la entrada del usuario.

## Cómo hacerlo:
Para convertir una cadena de texto a minúsculas en Fish Shell, podemos utilizar el comando `string lower`. Por ejemplo:

```
Fish Shell> set mi_cadena "HOLA MUNDO"
Fish Shell> echo $mi_cadena
HOLA MUNDO
Fish Shell> string lower $mi_cadena
Fish Shell> echo $mi_cadena
hola mundo
```

## Profundizando:
La conversión de una cadena de texto a minúsculas puede ser útil al trabajar con diferentes sistemas operativos, ya que algunos son sensibles a mayúsculas y minúsculas en las cadenas de texto. También puede ser necesario para comparar cadenas de texto de manera adecuada, ya que no todas las letras mayúsculas y minúsculas son iguales (como en idiomas que tienen caracteres especiales). 

Existen varias formas de convertir una cadena de texto a minúsculas en diferentes lenguajes y shells, como el comando `tr` en Bash. En Fish Shell, el comando `string lower` utiliza la función `tolower()` de C para realizar la conversión.

## Ver también:
- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [Bash: tr command](https://www.geeksforgeeks.org/tr-command-in-unix-linux-with-examples/)
- [C: tolower() function](https://www.geeksforgeeks.org/c-tolower-function/)