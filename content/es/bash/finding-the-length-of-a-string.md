---
title:                "Calculando la longitud de una cadena"
html_title:           "Bash: Calculando la longitud de una cadena"
simple_title:         "Calculando la longitud de una cadena"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué encontrar la longitud de una cadena?

En algunas ocasiones, es necesario saber cuántos caracteres tiene una cadena de texto, ya sea para validar su longitud o para realizar operaciones específicas en ella. En este artículo, te mostraremos cómo puedes encontrar fácilmente la longitud de una cadena en Bash.

## Cómo hacerlo

En Bash, podemos utilizar la función `expr` para encontrar la longitud de una cadena. Esta función toma una expresión como argumento y devuelve el resultado. Para encontrar la longitud de una cadena, podemos utilizar la expresión `length`, seguida de la cadena entre comillas.

```Bash
cadena="Hola mundo"
longitud=`expr length "$cadena"`
echo "La longitud de la cadena es $longitud" # Salida: La longitud de la cadena es 10
```

También podemos utilizar la sintaxis de la expansión de parámetros `${#cadena}` para encontrar la longitud de una cadena. Esta sintaxis devuelve la longitud de la cadena sin necesidad de utilizar la función `expr`.

```Bash
longitud="${#cadena}"
echo "La longitud de la cadena es $longitud" # Salida: La longitud de la cadena es 10
```

Si queremos encontrar la longitud de una cadena ingresada por el usuario, podemos utilizar el comando `read` para capturar la entrada y luego utilizar la sintaxis de la expansión de parámetros como se muestra anteriormente.

```Bash
echo "Ingresa una cadena:"
read cadena
longitud="${#cadena}"
echo "La longitud de la cadena es $longitud"
```

## Profundizando

En Bash, las cadenas de texto se pueden tratar como arreglos de caracteres. Esto significa que podemos acceder a cada caracter de una cadena utilizando su posición en el arreglo. La función `expr` también puede ser utilizada para encontrar la posición de un caracter específico en una cadena utilizando la expresión `index`.

```Bash
cadena="Hola mundo"
posicion=`expr index "$cadena" m` # Devuelve la posición de la primera "m" en la cadena
echo "La posición de la primera \"m\" es $posicion" # Salida: La posición de la primera "m" es 5
```

Además, podemos utilizar la expresión `substr` para obtener una subcadena de una cadena dada, especificando la posición inicial y la longitud deseada.

```Bash
cadena="Hola mundo"
subcadena=`expr substr "$cadena" 2 5` # Devuelve la subcadena desde la posición 2 hasta 5
echo "La subcadena es \"$subcadena\"" # Salida: La subcadena es "ola m"
```

## Ver también

 - [Documentación de Bash: String Manipulation](https://www.gnu.org/software/bash/manual/html_node/String-Manipulation.html)
 - [Expansión de parámetros en Bash](https://www.lifewire.com/using-bash-parameter-expansion-4028097)
 - [Explorando los arreglos en Bash](https://www.linuxjournal.com/content/bash-arrays)