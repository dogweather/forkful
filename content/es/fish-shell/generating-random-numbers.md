---
title:                "Generando números aleatorios"
html_title:           "Fish Shell: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

¡Hola Amigos!

En este artículo, vamos a hablar sobre una función muy útil en los programas, la generación de números aleatorios. Si eres programador, seguramente has utilizado o al menos has escuchado sobre esta función. ¿Pero sabes realmente qué es y por qué la usamos? ¡Vamos a descubrirlo!

## ¿Qué y por qué?

Generar números aleatorios es simplemente generar números de manera impredecible. Esto significa que cada vez que ejecutamos nuestro programa, los números que se generan serán diferentes. ¿Por qué hacemos esto? Bueno, hay muchas razones. Por ejemplo, podemos usarlos para generar datos de prueba, crear juegos, algoritmos de encriptación y mucho más. ¡Las posibilidades son infinitas!

## Cómo hacerlo:

En el shell de Fish, podemos generar números aleatorios utilizando la función `rand` seguida del máximo y mínimo de los números que queremos que se generen. Por ejemplo, si queremos generar un número entre 1 y 10, escribiríamos `rand 1 10` y presionaríamos Enter. Cada vez que ejecutamos esa línea, obtendremos un número diferente entre 1 y 10. Aquí hay un ejemplo de código y su salida:

```Fish Shell
$ rand 1 10
5
$ rand 1 10
9
$ rand 1 10
3
```

También podemos generar múltiples números aleatorios a la vez utilizando la función `seq` junto con `rand`. Por ejemplo, si queremos generar 5 números aleatorios entre 1 y 100, escribiríamos `seq 1 5 | xargs -I{} rand 1 100` y presionaríamos Enter. Aquí hay un ejemplo de código y su salida:

```Fish Shell
$ seq 1 5 | xargs -I{} rand 1 100
17
62
43
91
8
```

¡Fácil, ¿verdad?

## Deep Dive:

La generación de números aleatorios ha sido un tema muy discutido y estudiado en la informática. Uno de sus usos más conocidos es en la criptografía, donde se utilizan generadores de números pseudoaleatorios. Estos generadores siguen un algoritmo predefinido para generar una secuencia de números que parecen aleatorios, pero en realidad son determinísticos. Otra alternativa es utilizar fuentes de ruido físico, como el ruido atmosférico, para generar números realmente aleatorios.

Pero en el caso de Fish Shell, la función `rand` utiliza un generador pseudoaleatorio basado en la función `rand()` de C. Esta función utiliza el tiempo actual del sistema para calcular un número aleatorio. Por lo tanto, para obtener una mayor variedad de números, es recomendable ejecutar la función `rand` varias veces en un corto periodo de tiempo.

## Ver también:

- [Documentación oficial de la función `rand` en Fish Shell](https://fishshell.com/docs/current/cmds/rand.html)
- [Artículo sobre generadores de números aleatorios en Wikipedia](https://es.wikipedia.org/wiki/Generador_de_n%C3%BAmeros_aleatorios)
- [Tutorial sobre la generación de números aleatorios en C](https://www.educative.io/edpresso/generating-random-numbers-in-c)

¡Eso es todo por hoy! Espero que ahora entiendas mejor la generación de números aleatorios y cómo utilizarla en Fish Shell. ¡Hasta la próxima!