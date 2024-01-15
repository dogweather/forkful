---
title:                "Escribiendo en el error estándar"
html_title:           "Bash: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir en la salida de error estándar?

Escribir en la salida de error estándar es una práctica común en la programación de Bash. Es útil para identificar y manejar errores o eventos inesperados durante la ejecución de un script.

## Cómo hacerlo

Para escribir en la salida de error estándar en Bash, simplemente debemos utilizar el descriptor de archivo `2` seguido de `>` o `&>` para redireccionar la salida a la pantalla de error. Por ejemplo:

```Bash
echo "Esto será impreso en la salida estándar"
echo "Esto será impreso en la salida de error estándar" >&2
```

La primera línea se imprimirá en la pantalla, mientras que la segunda línea se imprimirá en la pantalla de error.

## Profundizando

Además de redireccionar la salida a la pantalla de error, también podemos guardarla en un archivo utilizando el descriptor de archivo `2` seguido de `> nombreArchivo`. Esto nos permite guardar y revisar los errores en un momento posterior.

Otra forma útil de utilizar la salida de error estándar es a través del comando `&>>`, que permite redireccionar tanto la salida estándar como la de error a un mismo archivo.

Es importante tener en cuenta que la salida de error estándar es diferente de la salida estándar, ya que se utiliza específicamente para los mensajes de error. Por lo tanto, es importante utilizar la salida adecuada para cada situación.

## Ver también

- [Guía de Bash para principiantes](https://www.linux.com/training-tutorials/writing-simple-bash-script/)
- [Redirección de salida y errores en Bash](https://www.digitalocean.com/community/tutorials/how-to-use-bash-s-redirection-operators)