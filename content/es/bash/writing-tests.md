---
title:                "Bash: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir tests en Bash

Los tests son una parte importante del proceso de programación en cualquier lenguaje, incluyendo Bash. Al escribir tests, podemos asegurarnos de que nuestro código funciona correctamente y detectar errores antes de que se conviertan en problemas mayores en un entorno de producción.

## Cómo escribir tests en Bash

Para escribir tests en Bash, podemos utilizar el comando `test` junto con expresiones lógicas y operadores de comparación. Por ejemplo, si queremos probar si una variable `$num` es igual a 10, podemos escribir lo siguiente dentro de un script:

```Bash
test $num -eq 10
```

Si ejecutamos el script y la variable es de hecho igual a 10, no se imprimirá nada en la salida. Sin embargo, si la variable es diferente a 10, obtendremos el siguiente mensaje de error:

```
test: 7: expected integer argument
```

Otros operadores de comparación útiles incluyen `-ne` (diferente de), `-lt` (menor que), `-gt` (mayor que) y `-le` (menor o igual a). También podemos utilizar expresiones lógicas como `&&` (y), `||` (o) y `!` (no) para crear tests más complejos.

## Profundizando en la escritura de tests

Además de las expresiones lógicas y operadores de comparación, también podemos utilizar condicionales como `if` y `elif` en nuestros tests. Por ejemplo, si queremos probar si una variable es un número menor a 10 y luego imprimir un mensaje si así lo es, podemos escribir lo siguiente:

```Bash
if [ $num -lt 10 ]; then
    echo "La variable es un número menor a 10."
elif [ $num -lt 20 ]; then
    echo "La variable es un número mayor o igual a 10, pero menor a 20."
else
    echo "La variable es un número igual o mayor a 20."
fi
```

También podemos utilizar ciclos `for` y `while` para ejecutar tests en un conjunto de valores o mientras una condición sea verdadera, respectivamente.

En resumen, escribir tests en Bash nos permite verificar rápidamente la funcionalidad de nuestro código y encontrar problemas antes de que afecten a un entorno de producción. Con la combinación correcta de expresiones lógicas, operadores de comparación y condicionales, podemos crear tests robustos y eficientes para nuestras aplicaciones.

## Ver también

- [Bash scripting tutorial](https://linuxize.com/post/bash-scripting-tutorial/)
- [Practical guide to Bash shell scripting](https://www.tecmint.com/bash-scripting-guide/)
- [Advanced Bash scripting guide](http://www.tldp.org/LDP/abs/html/index.html)