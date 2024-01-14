---
title:                "Fish Shell: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una herramienta muy útil en la programación para realizar tareas como crear contraseñas seguras, simular eventos aleatorios o generar datos de pruebas. En este blog post, te mostraremos cómo puedes generar números aleatorios en Fish Shell de una manera sencilla y eficiente.

## Cómo

````Fish Shell
#Generar un número entero aleatorio entre 0 y 10
set random_num (random 11)
echo $random_num

#Generar un número decimal aleatorio entre 0 y 1
set random_dec (math 1 \* (rand -g 1))
echo $random_dec

#Generar un número aleatorio entre un rango personalizado
set min 50
set max 100
set random_num (random $min $max)
echo $random_num
````

En el primer ejemplo, utilizamos la función `random` para generar un número entero aleatorio entre 0 y 10. Luego, lo imprimimos en pantalla utilizando el comando `echo`. 

En el segundo ejemplo, utilizamos la función `math` junto con `rand` para generar un número decimal aleatorio entre 0 y 1. Multiplicamos el resultado por 1 para asegurarnos de que el número sea decimal y utilizamos el modificador `-g` para indicar que queremos un número entre 0 y 1 en lugar de un número entero.

Finalmente, en el tercer ejemplo, utilizamos variables para especificar un rango personalizado y generar un número aleatorio entre ese rango. Puedes ajustar el rango cambiando los valores de las variables `min` y `max`.

## Deep Dive

Fish Shell utiliza el generador de números pseudoaleatorios `mt19937` de clase Mersenne Twister para generar números aleatorios. Este generador es considerado uno de los más fiables y eficientes en la generación de secuencias de números aleatorios.

Para obtener un número entero aleatorio entre 0 y N, Fish Shell utiliza la función `mt_rand` que toma como argumento el número superior del rango. Por ejemplo, si queremos generar un número aleatorio entre 0 y 10, utilizaremos `mt_rand(11)`. Luego, la función `rand` utiliza los números generados por `mt_rand` y los escala al rango deseado.

## Ver también

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/cmds/rand.html)
- [Mersenne Twister en Wikipedia](https://es.wikipedia.org/wiki/Mersenne_Twister)
- [Tutorial de Fish Shell en español](https://medium.com/@alesanchezr/tutorial-de-fish-shell-en-espa%C3%B1ol-22134da415f5)