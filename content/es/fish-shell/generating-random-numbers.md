---
title:                "Fish Shell: Generando números aleatorios"
programming_language: "Fish Shell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

¡Bienvenidos programadores a este nuevo artículo sobre programación en Fish Shell! En esta ocasión, hablaremos sobre algo muy útil e interesante: cómo generar números aleatorios en nuestro código. En esta publicación, aprenderemos por qué es importante utilizar esta función, cómo implementarla en Fish Shell y profundizaremos en cómo funciona. ¡Así que comencemos!

## Por qué
Generar números aleatorios puede ser muy útil en programación. Puede ser utilizado para crear contraseñas aleatorias, tomar decisiones aleatorias en un juego o para cualquier aplicación que requiera un elemento de aleatoriedad. Además, en el mundo de la programación, siempre es importante tener herramientas versátiles y la generación de números aleatorios es definitivamente una de ellas.

## Cómo
En Fish Shell, podemos utilizar el comando `math random` para generar números aleatorios. Por ejemplo, si queremos generar un número aleatorio entre 1 y 10, simplemente escribimos lo siguiente:

```
fish
set numero (math random 1 10)
echo Numero aleatorio: $numero
```

El resultado se verá así:

```
Numero aleatorio: 7
```

Podemos incluso utilizar esta función para generar contraseñas aleatorias, ya que podemos especificar el rango de caracteres que queremos incluir. Por ejemplo, si queremos una contraseña con 8 caracteres alfanuméricos, podemos escribir lo siguiente:

```
fish
set password (math random -c alphanum 8)
echo Contraseña aleatoria: $password
```

El resultado podría ser algo como esto:

```
Contraseña aleatoria: CSV3y2p9
```

¡Y así de simple es generar números aleatorios en Fish Shell!

## Profundizando
Si quieres entender más a fondo cómo funciona la generación de números aleatorios en Fish Shell, aquí te dejo algunos detalles interesantes.

- El comando `math random` utiliza una implementación de la librería C del algoritmo de generación de números pseudoaleatorios de Mersenne Twister. Esto significa que los números generados no son verdaderamente aleatorios, sino que siguen un patrón predecible a través de una semilla específica.
- Se pueden especificar diferentes opciones de caracteres en el argumento `-c` para generar contraseñas con distintos tipos de caracteres como letras, números, símbolos, entre otros.
- La semilla utilizada por defecto en `math random` se basa en el tiempo del sistema, pero también se puede especificar una semilla personalizada con el argumento `-s`.

## Ver también
- [Documentación oficial de Fish Shell sobre el comando `math random`](https://fishshell.com/docs/current/cmds/random.html)
- [Artículo sobre la generación de números aleatorios en Shell Script](https://www.linuxjournal.com/content/random-numbers-bash)
- [Otro ejemplo de cómo generar contraseñas aleatorias en Fish Shell](https://stackoverflow.com/questions/8069062/how-to-create-a-random-string-using-fish-shell)

¡Y eso es todo por hoy! Espero que hayan aprendido algo nuevo y que puedan incorporar la generación de números aleatorios en sus proyectos futuros. ¡Hasta la próxima, programadores!