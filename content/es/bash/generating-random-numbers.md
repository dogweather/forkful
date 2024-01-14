---
title:                "Bash: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Por qué generar números aleatorios?

Generar números aleatorios es una técnica comúnmente utilizada en la programación para realizar diferentes tareas, como simular eventos, generar claves de acceso seguras o crear juegos aleatorios. En esta publicación, aprenderemos cómo generar números aleatorios en Bash y profundizaremos en cómo funciona esta técnica.

## Cómo hacerlo

Para generar números aleatorios en Bash, utilizaremos el comando `shuf`. Este comando nos permite crear una secuencia aleatoria de números o líneas a partir de un archivo o de la entrada estándar. Veamos un ejemplo de cómo generar 5 números aleatorios del 1 al 10:

```bash
shuf -i 1-10 -n 5
```
Output:
```
8
3
2
9
6
```

También podemos utilizar el comando `seq` para generar una secuencia de números y luego utilizar `shuf` para mezclarlos de manera aleatoria:

```bash
shuf <(seq 1 10) -n 5
```
Output:
```
7
2
9
5
1
```

Otra opción es utilizar la variable de entorno `$RANDOM`, que genera un número aleatorio del 0 al 32767 cada vez que se llama:

```bash
echo $RANDOM
```
Output:
```
23765
```

## Profundizando en la generación de números aleatorios

Es importante tener en cuenta que los números "aleatorios" generados por las computadoras no son realmente aleatorios, sino que se basan en algoritmos matemáticos que utilizan una semilla o "seed" para determinar el resultado. Por lo tanto, si utilizamos el mismo comando con la misma semilla, obtendremos el mismo resultado.

Por defecto, `shuf` utiliza la variable de entorno `$RANDOM` como semilla, pero si queremos obtener números diferentes cada vez, podemos especificar una semilla diferente utilizando el parámetro `-i` seguido de un número. Por ejemplo:

```bash
shuf -i 1-10 --random-source=/dev/random -n 5
```
Output:
```
7
5
1
4
9
```

También podemos generar números aleatorios con decimales utilizando el comando `awk`:

```bash
awk 'BEGIN{srand();print rand()}'
```
Output:
```
0.631997
```

Además de generar números aleatorios, también podemos generar caracteres aleatorios utilizando el comando `tr`, que nos permite convertir o eliminar caracteres en un archivo o en la entrada estándar.

```bash
cat /dev/urandom | tr -dc A-Za-z0-9 | head -c 10
```
Output:
```
m9s5ZlgwoK
```

## Ver también
- [Guía de Bash para principiantes](https://ubuntu.com/tutorials/command-line-for-beginners#2-opening-a-terminal)
- [Documentación de Bash en español](https://www.gnu.org/software/bash/manual/bash.html#Invoking-Bash)
- [Generación de números aleatorios en Python](https://www.w3schools.com/python/ref_random_randint.asp)