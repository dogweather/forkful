---
title:                "Interpolando una cadena"
html_title:           "Fish Shell: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Interpolación de una cadena es una técnica utilizada por programadores para insertar valores dinámicamente en una cadena de texto. Esto permite crear cadenas de forma más dinámica y eficiente, ya que no es necesario escribir cada valor de forma estática.

## Cómo hacerlo:
La sintaxis para la interpolación de cadenas en Fish Shell es utilizando el signo de dólar ($), seguido de llaves ({}) y dentro de estas llaves, el valor que queremos insertar. Por ejemplo:

```
set name "Juan"
echo "¡Hola, ${name}!"
```

Esto imprimirá "¡Hola, Juan!" en la consola. También podemos realizar operaciones dentro de las llaves, por ejemplo:

```
set num 5
echo "El resultado es: $(math $num * 2)"
```

Esto imprimirá "El resultado es: 10" en la consola.

## Inmersión profunda:
La interpolación de cadenas ha sido utilizada por diferentes lenguajes de programación durante años, como Perl o Ruby. En otros lenguajes, en lugar del signo de dólar, se utiliza el símbolo de porcentaje (%) para indicar que se va a insertar un valor.

Existen también otras formas de insertar valores en cadenas, como utilizar la función `printf`, pero la interpolación de cadenas en Fish Shell es una forma más simple y directa de lograr el mismo resultado.

En cuanto a la implementación en Fish Shell, utiliza la función `string interpolation` para realizar esta acción de forma eficiente.

## Ver también:
- [Documentación oficial de Fish Shell sobre la interpolación de cadenas](https://fishshell.com/docs/current/cmds/string.html)
- [Ejemplos de interpolación de cadenas en Fish Shell](https://gist.github.com/justintung/5231199)