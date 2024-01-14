---
title:                "Bash: Encontrando la longitud de una cadena"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué buscar la longitud de una cadena en Bash?

En la programación, a menudo nos encontramos con la necesidad de conocer la longitud de una cadena de texto. Ya sea para validar entradas de usuario, formatear datos o realizar operaciones específicas, es importante poder obtener esta información de manera rápida y precisa. En Bash, existen varias formas de encontrar la longitud de una cadena, y en este artículo te enseñaré cómo hacerlo fácilmente.

## Cómo hacerlo en Bash

Para encontrar la longitud de una cadena en Bash, podemos usar la función "expr length" o el comando "wc" (word count). Por ejemplo, si queremos conocer la longitud de la cadena "hola", podemos escribir lo siguiente en la terminal:

```Bash
expr length "hola" 
```

El resultado que obtendríamos es 4, ya que la cadena tiene 4 caracteres. También podemos usar el comando "wc" de la siguiente manera:

```Bash
echo -n "hola" | wc -c
```

El resultado será el mismo, ya que "wc -c" cuenta el número de caracteres en la entrada.

## Profundizando en la longitud de una cadena

Es importante tener en cuenta que en Bash, los espacios en blanco también se consideran caracteres. Por lo tanto, si tenemos una cadena como "hola mundo", la longitud será de 11 caracteres, ya que hay un espacio entre ambas palabras.

También podemos usar la expansión de parámetros "${#variable}" para obtener la longitud de una cadena almacenada en una variable. Por ejemplo, si tenemos la variable "nombre" con el valor "Juan", podemos escribir lo siguiente para obtener su longitud:

```Bash
echo ${#nombre}
```

## Ver también

- [Documentación de expr](https://www.gnu.org/software/coreutils/manual/html_node/expr-invocation.html#expr-invocation)
- [Documentación de wc](https://www.gnu.org/software/coreutils/manual/html_node/wc-invocation.html#wc-invocation)