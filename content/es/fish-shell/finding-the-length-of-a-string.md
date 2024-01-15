---
title:                "Encontrando la longitud de una cadena"
html_title:           "Fish Shell: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Si estás programando en Fish Shell, es posible que en algún momento necesites saber la longitud de una cadena de texto. Esta información puede ser útil para realizar acciones específicas en tu programa o simplemente para mostrar información al usuario. En este artículo te explicaremos cómo puedes hacerlo de manera sencilla.

## Cómo hacerlo

Para obtener la longitud de una cadena de texto en Fish Shell, utilizaremos el comando `string length`. Este comando toma como argumento la cadena de texto de la cual queremos obtener la longitud, y devuelve un número que representa dicha longitud. Veamos un ejemplo en el que declaramos una variable con una cadena de texto y utilizamos el comando `string length` para obtener su longitud:

```Fish Shell
set cadena "¡Hola mundo!"
echo (string length $cadena)
```

La salida de este código será `12`, ya que la cadena de texto "¡Hola mundo!" tiene 12 caracteres. Fácil, ¿verdad?

## Profundizando

Es importante mencionar que Fish Shell cuenta con una serie de comandos y atajos que facilitan la manipulación de cadenas de texto. Algunos de ellos son `string sub`, que nos permite obtener una subcadena de una cadena original, y `string replace`, que nos permite reemplazar una parte de una cadena por otra. Además, Fish Shell también nos ofrece la posibilidad de utilizar expresiones regulares para manipular cadenas de texto de manera más avanzada.

En resumen, conocer la longitud de una cadena de texto puede ser de gran ayuda en nuestras tareas de programación, y gracias a los comandos y atajos de Fish Shell podemos hacerlo de manera rápida y sencilla.

## Ver también

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial de Fish Shell en español](https://medium.com/@guillermo_andreu/tutorial-de-fish-shell-en-espa%C3%B1ol-c5ae22ba9a28)
- [Expresiones regulares en Fish Shell](https://fishshell.com/docs/current/cmds/string.html#string-replace)