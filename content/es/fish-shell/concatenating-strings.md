---
title:                "Fish Shell: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una técnica importante en la programación que permite combinar múltiples cadenas de texto en una sola. Esto es útil para crear mensajes personalizados, mostrar datos de forma clara o simplemente para unir diferentes pedazos de texto. Aprender a concatenar cadenas en Fish Shell es una habilidad valiosa para cualquier programador. 

## Cómo hacerlo

```Fish Shell 
set nombre "María"
set apellido "García"
set nombre_completo $nombre" "$apellido
echo $nombre_completo
```

Este código define dos variables, `nombre` y `apellido`, y luego las concatena con un espacio en blanco en la variable `nombre_completo`. Al usar el commando `echo`, podemos imprimir el valor de `nombre_completo` en la terminal, que mostrará "María García".

Esta técnica también funciona con cadenas de caracteres, no solo con variables, como se muestra a continuación:

```Fish Shell 
set mensaje "¡Hola! Mi nombre es "
set nombre "Juan"
echo $mensaje$nombre
```

Este código resultará en "¡Hola! Mi nombre es Juan" impreso en la terminal. Como puedes ver, podemos combinar variables y cadenas de texto para crear mensajes personalizados.

## Una mirada más profunda

La concatenación de cadenas en Fish Shell se realiza utilizando el operador `+` o utilizando la sintaxis de `$variable1$variable2` como vimos en los ejemplos anteriores. Además, también se pueden usar múltiples operadores en una sola expresión, lo que permite la concatenación de más de dos cadenas.

Otro aspecto importante a tener en cuenta es que al concatenar cadenas de texto, no se agrega ningún espacio entre ellas automáticamente, por lo que es necesario agregar espacios en blanco de forma manual si es necesario.

## Ver también

- [Guía de referencia básica de Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Documentación de concatenación de cadenas en Fish Shell](https://fishshell.com/docs/current/cmds/set.html#cmd-set)

¡Espero que hayas aprendido cómo concatenar cadenas en Fish Shell con este artículo! ¡Ahora podrás utilizar esta habilidad en tus proyectos de programación y mejorar la funcionalidad de tus scripts!