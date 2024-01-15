---
title:                "Extrayendo subcadenas"
html_title:           "Bash: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Extraer subcadenas de una cadena de caracteres es una habilidad útil que te permitirá realizar diversas tareas en Bash, como manipular archivos, extraer información específica de un texto o validar entradas de usuario.

## Cómo hacerlo

Extraer una subcadena de una cadena en Bash es muy sencillo. Solo necesitas seguir los siguientes pasos:

1. Identifica la cadena de donde quieres extraer la subcadena.
2. Utiliza el comando ```substring``` seguido del índice inicial y la longitud deseada de la subcadena, separados por ```:```.
3. Ejecuta el comando y la subcadena será mostrada en la terminal.

Por ejemplo, si tenemos la cadena ```"Hola Mundo"``` y queremos extraer la subcadena ```"Mundo"```, podemos utilizar el siguiente código:

```Bash
cadena="Hola Mundo"
echo ${cadena:5:5}
```

En este caso, el resultado sería ```Mundo```, ya que el índice inicial es 5 y la longitud de la subcadena es 5.

Es importante tener en cuenta que en Bash, los índices comienzan desde 0, por lo que si queremos extraer la subcadena ```"Hola"``` de ```"Hola Mundo"```, tendríamos que utilizar el comando ```substring``` de la siguiente manera:

```Bash
cadena="Hola Mundo"
echo ${cadena:0:4}
```

El resultado sería ```Hola```, ya que empezamos en el índice 0 y la longitud de la subcadena es 4.

## Profundizando

Además de utilizar el comando ```substring```, también podemos utilizar diferentes métodos para extraer subcadenas en Bash. Por ejemplo, podemos utilizar el comando ```cut``` para extraer una sección de una cadena utilizando un delimitador específico.

También podemos utilizar comodines, como el asterisco ```*```, para extraer subcadenas que cumplan con un patrón determinado.

Es importante familiarizarse con estos diferentes métodos y encontrar el que mejor se adapte a tus necesidades.

## Ver también

- [Documentación oficial de Bash](https://www.gnu.org/software/bash/)
- [Manipulación de cadenas en Bash](https://www.linux.com/training-tutorials/bash-string-manipulation-beginners-examples/)