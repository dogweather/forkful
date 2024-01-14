---
title:    "Fish Shell: Generando números aleatorios"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una práctica común en la programación, ya que puede ayudar a realizar pruebas, crear juegos o simplemente agregar una pequeña dosis de imprevisibilidad a tu código.

## Cómo hacerlo

Para generar números aleatorios en Fish Shell, puedes utilizar la función `random` junto con la operación de expansión `{}` para especificar el rango de números que quieres obtener.

```
Fish Shell

# Generar un número aleatorio entre 1 y 10
set rand_number (random 1 10)

# Imprimir el número generado
echo "Mi número aleatorio es {}rand_number"
```

El resultado de este código será un número aleatorio entre 1 y 10, que se imprimirá en la terminal cuando ejecutes el script.

## Profundizando

Existen diferentes formas de generar números aleatorios en Fish Shell, dependiendo de tus necesidades. Por ejemplo, si necesitas un número decimal en lugar de un número entero, puedes utilizar la función `math` junto con `random` para obtener un número con decimales. También puedes utilizar la bandera `-l` para obtener un número aleatorio en una línea diferente cada vez que se ejecute el script.

Además, puedes utilizar los comandos `seq` o `shuf` en combinación con `random` para generar una secuencia de números aleatorios o mezclar una lista de números específicos.

¡Experimenta con diferentes combinaciones y descubre cómo puedes incorporar números aleatorios en tus proyectos de programación!

## Ver también

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Lista de comandos de Fish Shell](https://fishshell.com/docs/current/commands.html)
- [Guía de iniciación rápida en Fish Shell](https://fishshell.com/docs/current/tutorial.html)