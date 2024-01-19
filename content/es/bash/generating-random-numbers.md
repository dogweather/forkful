---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
La generación de números aleatorios es el proceso de producir una sucesión de números sin un patrón predecible. Los programadores lo hacen para variar la ejecución del programa, especialmente para las pruebas y los juegos.

## Cómo hacerlo:

Para generar un número aleatorio en Bash, usamos la variable especial `RANDOM`. Cada vez que se llama a `RANDOM`, devuelve un número aleatorio entre 0 y 32767.

```Bash
echo $RANDOM
```

Si deseas un rango diferente, puedes utilizar un módulo. Por ejemplo, para obtener un número aleatorio entre 0 y 99, usa `RANDOM % 100`:

```Bash
echo $(( RANDOM % 100 ))
```

El output para los comandos previos será un número aleatorio.

## Inmersión Profunda:

Historicalmente, generar números aleatorios en computadoras ha sido un desafío debido a su naturaleza determinista. Sin embargo, con herramientas como `RANDOM` en Bash, esto se ha simplificado. Aunque `RANDOM` no es adecuado para todas las situaciones, como la criptografía.

Existen alternativas a `RANDOM`, como `/dev/urandom` en sistemas Unix, o la función `rand()` en lenguajes de programación más avanzados como Python o JavaScript.

La implementación interna de `RANDOM` en Bash utiliza una fórmula para calcular el próximo número en la secuencia cada vez que se llama. Aunque esto es pseudoaleatorio, es suficiente para la mayoría de los usos generales.

## Ver También:

Para aprender más sobre la generación de números aleatorios en Bash, visite:

- Guía de Bash: https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_02_03.html
- Manual de Bash: https://www.gnu.org/software/bash/manual/bash.html#Shell-Arithmetic
- Generación de números aleatorios: https://en.wikipedia.org/wiki/Random_number_generation