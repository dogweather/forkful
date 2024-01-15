---
title:                "Escribiendo pruebas"
html_title:           "Bash: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué

Si estás escribiendo un script en Bash, es importante asegurarse de que todo funciona correctamente. Una forma de garantizar esto es a través de pruebas. Las pruebas te ayudan a identificar y corregir errores en tu código antes de ponerlo en producción.

## Cómo

Las pruebas en Bash se realizan a través del comando `test` o su forma abreviada `[ ]`. Aquí hay un ejemplo de cómo realizar una prueba en la que se comprueba si dos variables son iguales:

```Bash
#!/bin/bash

var1="hola"
var2="mundo"

if [ $var1 == $var2 ]
then
    echo "Las variables son iguales"
else
    echo "Las variables son diferentes"
fi
```
Este código imprimirá "Las variables son diferentes" ya que `var1` y `var2` no son iguales. Puedes realizar diversas pruebas utilizando operadores lógicos como `==`, `!=`, `-eq`, `-ne`, `-gt`, `-lt`, entre otros.

## Deep Dive

Además de las pruebas básicas, también puedes crear pruebas más complejas utilizando los comandos `and` y `or`. Por ejemplo, puedes utilizar el comando `and` para verificar simultáneamente si dos o más condiciones son verdaderas y utilizar `or` para comprobar si al menos una de las condiciones es verdadera.

Otra técnica útil es el uso de expresiones regulares en las pruebas. Por ejemplo, si quieres verificar si una variable contiene sólo números, puedes utilizar el operador `=~` junto con una expresión regular:

```Bash
#!/bin/bash

var="12345"

if [[ $var =~ ^[0-9]+$ ]]
then
    echo "La variable contiene sólo números"
else
    echo "La variable no contiene sólo números"
fi
```

Existen muchas otras técnicas y comandos que pueden ser utilizados en las pruebas de Bash, es importante investigar y practicar para aprovechar al máximo esta herramienta.

## Ver también

- [Bash Guide for Beginners](https://linux.die.net/Bash-Beginners-Guide)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/index.html)