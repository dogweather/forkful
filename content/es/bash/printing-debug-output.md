---
title:                "Imprimiendo salida de depuración"
html_title:           "Bash: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Por qué imprimir la salida de depuración?

Imprimir la salida de depuración puede ser una herramienta útil para solucionar problemas en nuestro código de Bash. Al imprimir información adicional, podemos analizar qué está sucediendo en nuestro programa y encontrar posibles errores o fallos en la lógica.

## Cómo hacerlo

Imprimir la salida de depuración es muy sencillo en Bash. Simplemente necesitamos utilizar el comando `echo` y agregar el texto que queremos imprimir dentro de las comillas. Por ejemplo:

```Bash
echo "Imprimiendo salida de depuración"
```

Al ejecutar este comando, veremos el texto impreso en la consola. Sin embargo, también podemos utilizar el parámetro `-e` para imprimir variables o caracteres especiales. Por ejemplo:

```Bash
numero=10
echo -e "El número es: $numero \nFin de la salida de depuración"
```

Este comando imprimirá el valor de la variable `numero` y también añadirá una nueva línea al final del texto. Esto puede ser útil para una salida más organizada y legible.

## Profundizando en la impresión de salida de depuración

Además de utilizar `echo`, también podemos utilizar otros comandos como `printf`, `print`, o `logger` para imprimir la salida de depuración en Bash. Cada uno de estos comandos tiene sus propias características y opciones adicionales que podemos utilizar según nuestras necesidades.

También es importante tener en cuenta que, al imprimir la salida de depuración, debemos tener cuidado de no incluir información confidencial o sensible. Una buena práctica es utilizar una variable especial como `DEBUG` que podamos activar o desactivar fácilmente en nuestro código, para no imprimir información innecesaria en producción.

## Ver también

- [Guía de usuario de Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Documentación de comandos en GNU/Linux](https://www.tldp.org/LDP/abs/html/index.html)