---
title:                "Fish Shell: Concatenando cadenas"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Por qué
Muchas veces, al escribir un programa en Fish Shell, necesitamos combinar o unir varias cadenas de texto para formar una nueva, ya sea para mostrar información al usuario o para almacenarla en una variable. Por eso, aprender a concatenar cadenas es una habilidad esencial para cualquier programador en Fish Shell.

## Cómo hacerlo
Para concatenar cadenas en Fish Shell, utilizamos el operador `+`. Esto nos permite unir dos o más cadenas y obtener una nueva cadena combinada. Veamos un ejemplo:

```fish
set nombre "Juan"
set apellido "Pérez"
set nombre_completo $nombre + " " + $apellido
echo $nombre_completo
```
Salida: `Juan Pérez`

En este ejemplo, hemos creado dos variables, `nombre` y `apellido`, y luego las hemos concatenado con el operador `+` para crear una nueva variable `nombre_completo`. Al imprimir su valor, obtenemos la cadena combinada "Juan Pérez".

También podemos concatenar más de dos cadenas a la vez. En ese caso, el operador `+` se utilizará para unir la primera y segunda cadena, y luego el resultado se unirá con la tercera cadena, y así sucesivamente. Por ejemplo:

```fish
set saludo "Hola,"
set nombre "Juan"
set apellido "Pérez"
set despedida "¡Hasta pronto!"
set mensaje $saludo + " " + $nombre + " " + $apellido + ". " + $despedida
echo $mensaje
```
Salida: `Hola, Juan Pérez. ¡Hasta pronto!`

Como se puede ver, hemos concatenado cuatro cadenas diferentes para formar un mensaje completo. Este es solo uno de los muchos ejemplos de cómo podemos utilizar la concatenación de cadenas en Fish Shell.

## Profundizando
Cuando concatenamos cadenas en Fish Shell, es importante tener en cuenta que todas las cadenas deben tener el mismo tipo de comillas. Por ejemplo, si una cadena comienza con comillas dobles, todas las demás deben ser del mismo tipo. Esto asegura que los espacios entre cadenas sean interpretados correctamente.

También hay un operador de concatenación abreviado, `+=`, que nos permite agregar una nueva cadena al final de una variable existente sin tener que volver a asignar su valor completo. Por ejemplo:

```fish
set nombre "Juan"
set apellido "Pérez"
set nombre_completo $nombre
echo $nombre_completo
set nombre_completo += " " + $apellido
echo $nombre_completo
```
Salida: `Juan`  
`Juan Pérez`

Esta es una forma útil de agregar información a una cadena existente sin tener que reescribir todo.

# Ver también
- [Fish Shell documentation on strings](https://fishshell.com/docs/current/tutorial.html#strings)
- [Fish Shell string manipulation](https://fishshell.com/docs/current/tutorial.html#string-manipulation)
- [More examples of string concatenation in Fish Shell](https://www.linuxtricks.fr/wiki/concatenation-de-chaines-de-caracteres-en-fish-shell)