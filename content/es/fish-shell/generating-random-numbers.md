---
title:    "Fish Shell: Generando números aleatorios"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una técnica útil en la programación para simular situaciones aleatorias y probar la funcionalidad de un programa. También puede utilizarse en juegos y aplicaciones de entretenimiento.

## Cómo hacerlo

Para generar números aleatorios en Fish Shell, podemos utilizar el comando `rand`. Por ejemplo, si queremos generar un número aleatorio entre 1 y 10, podemos escribir:

```Fish Shell
rand 1 10
```

Esto nos dará un número aleatorio cada vez que ejecutemos el comando. También podemos utilizar variables para almacenar los números aleatorios y utilizarlos en nuestro código. Ejemplo:

```Fish Shell
set numero (rand 1 10)
echo "Tu número aleatorio es $numero"
```

Podemos utilizar el comando `seq` para generar una secuencia de números aleatorios. Por ejemplo, si queremos generar 5 números aleatorios entre 1 y 100, podemos escribir:

```Fish Shell
seq (rand 1 100) 5
```

## Inmersión profunda

La generación de números aleatorios en Fish Shell utiliza el algoritmo `xorshift64`, el cual es un generador de números pseudoaleatorios. Esto significa que, aunque los números pueden parecer aleatorios, en realidad siguen un patrón determinado por el algoritmo.

También es importante destacar que la semilla utilizada para generar los números aleatorios puede ser especificada mediante el uso del comando `srand`. Esto nos permite obtener los mismos números aleatorios en ejecuciones futuras del programa, siempre y cuando utilicemos la misma semilla.

## Ver también

- [Documentación oficial de Fish Shell sobre números aleatorios](https://fishshell.com/docs/current/cmds/rand.html)
- [Generando números aleatorios con el comando seq en Fish Shell](https://www.mankier.com/1/seq)
- [Algoritmo xorshift en Wikipedia (en inglés)](https://en.wikipedia.org/wiki/Xorshift)