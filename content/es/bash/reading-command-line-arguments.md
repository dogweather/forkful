---
title:                "Bash: Leyendo argumentos de línea de comando"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

¿Te has preguntado alguna vez cómo los programas en la terminal reciben la información que les proporcionas? Una de las formas en las que esto ocurre es a través de los argumentos de línea de comando. En este artículo, te explicaremos por qué es importante saber leer y utilizar correctamente estos argumentos.

## Cómo hacerlo

Leer los argumentos de línea de comando puede parecer intimidante para aquellos que recién empiezan en la programación de la terminal. Pero en realidad, es muy sencillo. Para empezar, necesitas saber que los argumentos de línea de comando se pasan al programa como cadenas de texto y se pueden acceder a ellos a través de las variables especiales "$1", "$2", "$3", etc. Estas variables almacenan los diferentes argumentos en el orden en que se ingresaron.

Veamos un ejemplo. Supongamos que tenemos un programa llamado "saludo.sh" y queremos pasarle dos argumentos: un nombre y un saludo. El código se vería así:

```Bash
#!/bin/bash
echo "¡Hola $1! $2"
```

Si ejecutamos el programa de la siguiente manera:

```Bash
./saludo.sh Juan "Buenos días"
```

El resultado sería:

```Bash
¡Hola Juan! Buenos días
```

Como puedes ver, el primer argumento que colocamos ("Juan") se asigna a la variable "$1" y el segundo ("Buenos días") a la variable "$2".

Pero ¿qué pasa si queremos pasar más de dos argumentos? Podemos acceder a ellos utilizando un bucle "for". Por ejemplo:

```Bash
#!/bin/bash
echo "¡Hola $1! $2"
echo "También saludamos a:"
for arg in $@; do
  echo "- $arg"
done
```

En este caso, "$@" representa todos los argumentos pasados al programa. Entonces, si ejecutamos el programa con los argumentos "Juan", "Buenos días" y "María", el resultado sería:

```Bash
¡Hola Juan! Buenos días
También saludamos a:
- Juan
- Buenos días
- María
```

## Profundizando

Ahora que sabes cómo leer los argumentos de línea de comando y cómo utilizarlos en tu código, podemos profundizar un poco más en su funcionamiento. Los argumentos de línea de comando pueden ser opciones o parámetros. Las opciones suelen comenzar con un guión "-" y pueden tener un valor asociado después de un espacio, como en "-c 10". Los parámetros, en cambio, son valores directos sin ningún tipo de indicador.

Por ejemplo, si quisiéramos que nuestro programa "saludo.sh" también permita recibir un argumento opcional que indique cuántas veces queremos que se repita el saludo, podríamos utilizar la opción "-n" y un parámetro después de ella. El código se vería así:

```Bash
#!/bin/bash

nombre=$1
mensaje=$2
num_rep=1

# Utilizamos un bucle "while" para verificar si encuentra la opción "-n"
while getopts "n:" opt; do
  # Si encuentra la opción, asignamos el valor a la variable "num_rep"
  case $opt in
    n) num_rep=$OPTARG;;
  esac
done

# Utilizamos un bucle "for" para repetir el saludo según el valor de "num_rep"
for ((i=1; i<=$num_rep; i++)); do
  echo "¡Hola $nombre! $mensaje"
done
```

Ahora, si ejecutamos el programa con los argumentos "Juan", "Buenos días" y "-n 3", obtendremos el siguiente resultado:

```Bash
¡Hola Juan! Buenos días
¡Hola Juan! Buenos días
¡Hola Juan! Buenos días
```

## Ver también

- [Introducción a la programación de la terminal en Bash](https://www.freecodecamp.org/news/writing-a-bash-script-a-beginners-guide/)
- [Documentación oficial de Bash](https://www.gnu.org/software/bash/)
- [Tutorial de argumentos de línea de comando en Bash](https://ryanstutorials.net/bash-scripting-tutorial/bash-arguments.php)