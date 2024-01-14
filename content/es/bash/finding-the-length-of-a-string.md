---
title:                "Bash: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué
En la programación, a menudo nos encontramos trabajando con cadenas de caracteres (strings) y puede ser necesario saber su longitud. Esto puede ser útil al manipular datos o algoritmos que requieren una cantidad específica de caracteres. Aprender a encontrar la longitud de una cadena de caracteres es una habilidad útil para cualquier programador.

## Cómo hacerlo
Para encontrar la longitud de una cadena de caracteres en Bash, se puede utilizar el comando `expr length`, seguido de la cadena que se desea medir. Por ejemplo:

```Bash 
cadena="Hola, mundo!"

echo La longitud de la cadena es: `expr length $cadena` 
```

La salida de este código será: `La longitud de la cadena es: 12`, ya que la cadena contiene 12 caracteres.

## Profundizando en el tema
Además del comando `expr length`, también se puede utilizar el comando `echo`, seguido de la variable y de la expresión `#` para obtener la longitud de una cadena de caracteres. Por ejemplo:

```Bash
cadena="¡Hola, amigos!"

echo La longitud de la cadena es: ${#cadena} 
```

La salida de este código será: `La longitud de la cadena es: 14`, ya que la cadena contiene 14 caracteres.

Es importante tener en cuenta que los espacios en blanco también se cuentan como caracteres en una cadena de caracteres. Además, se puede utilizar la condición `if` junto con `expr length` para realizar acciones basadas en la longitud de una cadena en un script de Bash. 

## Ver también
- [Documentación oficial de Bash](https://www.gnu.org/software/bash/)
- [Tutorial de Bash para principiantes](https://programmingwithmosh.com/linux/bash-scripting-tutorial/)
- [Más comandos útiles de Bash](https://www.tldp.org/LDP/abs/html/refcards.html#AEN22832)

¡Ahora ya sabes cómo encontrar la longitud de una cadena de caracteres en Bash! Prueba diferentes comandos y técnicas para familiarizarte con ellos y utilizarlos en tus proyectos de programación. ¡Buenas suerte!