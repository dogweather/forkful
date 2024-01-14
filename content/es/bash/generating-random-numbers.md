---
title:    "Bash: Generando números aleatorios"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en Bash

El uso de números aleatorios puede ser útil en programación para simular situaciones aleatorias, como en juegos o pruebas de rendimiento. También puede ser una herramienta útil en la generación de contraseñas o en la selección aleatoria de elementos de una lista. En Bash, podemos utilizar la función "shuf" para generar números aleatorios.

## Cómo generar números aleatorios en Bash

Para generar un solo número aleatorio en Bash, podemos utilizar el siguiente comando:

```Bash
echo $RANDOM
```

El comando "echo" nos permite imprimir el valor de la variable "$RANDOM", que contiene un número aleatorio generado por el sistema.

Si queremos generar varios números aleatorios, podemos utilizar un bucle "for" y la función "shuf" de la siguiente manera:

```Bash
for i in {1..5}
do
    shuf -i 1-10 -n 1
done
```

En este ejemplo, el bucle se repetirá 5 veces, y en cada iteración utilizará la función "shuf" para obtener un número aleatorio entre 1 y 10.

## Profundizando en la generación de números aleatorios

En Bash, la función "shuf" utiliza el algoritmo de Fisher-Yates para generar una permutación aleatoria de los números en una lista determinada. Esto significa que cada número en la lista tiene la misma probabilidad de ser seleccionado, lo que garantiza una verdadera aleatoriedad.

Además de generar números aleatorios, también podemos utilizar la función "shuf" para mezclar los elementos de una lista en un orden aleatorio, simplemente agregando la opción "-n" y especificando la cantidad de elementos que queremos mezclar.

## Ver también

- [Documentación de GNU Bash sobre la función "shuf"](https://www.gnu.org/software/bash/manual/html_node/The-shuf-Built_002din.html)
- [Ejemplos de bucles en Bash](https://www.linuxjournal.com/content/bash-loops)
- [Más información sobre el algoritmo de Fisher-Yates](https://www.geeksforgeeks.org/shuffle-a-given-array-using-fisher-yates-shuffle-algorithm/)