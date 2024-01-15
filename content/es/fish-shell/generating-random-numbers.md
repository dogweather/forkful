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

## Por qué
¿Alguna vez has necesitado generar números aleatorios para tus proyectos de programación? Pues, en este artículo te enseñaré cómo hacerlo utilizando el Fish Shell, la versión actual de esta popular línea de comandos.

## Cómo hacerlo
Es muy sencillo generar números aleatorios en Fish Shell, gracias a su función incorporada `math` que nos permite realizar operaciones matemáticas.

Para generar un número aleatorio entero, podemos utilizar la función `random` seguida de la cantidad de dígitos que queremos obtener. Por ejemplo, si queremos obtener un número de 3 dígitos, escribiríamos en nuestra línea de comandos lo siguiente:

```Fish Shell
random 3 
```

El resultado podría ser, por ejemplo, `456`. Como puedes ver, cada vez que ejecutes este comando obtendrás un número aleatorio diferente.

También podemos generar un número real utilizando el mismo método, pero especificando la cantidad de dígitos decimales que queremos obtener. Por ejemplo:

```Fish Shell
random 5    # Genera un número real de 5 dígitos.
```

De esta forma podríamos obtener, por ejemplo, `8.97456` como resultado.

## Deep Dive
Ahora que ya sabes cómo generar números aleatorios en Fish Shell, es interesante profundizar un poco más en el tema.

Cuando utilizamos la función `random`, en realidad estamos generando un número pseudo-aleatorio, es decir, no es completamente aleatorio ya que se basa en una semilla que se utiliza para obtener el número. Esta semilla cambia cada vez que iniciamos Fish Shell, lo que nos garantiza una mayor variedad de resultados.

Además, al especificar la cantidad de dígitos que queremos obtener, debemos tener en cuenta que no podemos exceder el límite de nuestra arquitectura de computador. Por ejemplo, si intentamos generar un número de 20 dígitos en un sistema de 32 bits, nos dará un error ya que supera el límite de representación de números.

En resumen, la función `random` es una forma rápida y sencilla de generar números aleatorios en Fish Shell, pero es importante tener en cuenta sus limitaciones y cómo funciona en realidad para obtener resultados precisos.

## Ver también
- Documentación oficial de Fish Shell (https://fishshell.com/docs/current/index.html)
- Otras funciones útiles de Fish Shell (https://www.baeldung.com/linux/fish-shell-commands)
- Tutorial de programación en Fish Shell (https://www.freecodecamp.org/news/the-fish-shell-tutorial/)