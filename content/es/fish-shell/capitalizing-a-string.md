---
title:                "Fish Shell: Capitalizar una cadena"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena en programación?

Capitalizar una cadena significa convertir la primera letra de cada palabra en mayúscula. Aunque puede parecer una tarea sencilla, puede ser útil en ciertas situaciones, como formatear nombres o crear títulos.

## Cómo hacerlo en Fish Shell

Fish Shell es un lenguaje de programación sencillo y fácil de aprender. Para capitalizar una cadena en Fish Shell, podemos utilizar el comando `string capitalize` seguido de la cadena que queremos capitalizar. Por ejemplo:

```Fish Shell
string capitalize "hola, ¿cómo estás?"
```

La salida de este comando sería "Hola, ¿Cómo Estás?". Como podemos ver, la primera letra de cada palabra ha sido convertida en mayúscula.

## Profundizando en la capitalización de cadenas

Aunque el comando `string capitalize` es útil en la mayoría de los casos, también existen otras formas de capitalizar una cadena en Fish Shell. Por ejemplo, podemos utilizar el comando `string sub` para reemplazar la primera letra de cada palabra por su versión mayúscula. Esto nos da un mayor control sobre cómo queremos capitalizar la cadena.

Otra opción es utilizar la función `join` para combinar una lista de palabras capitalizadas en una sola cadena. Esto puede ser útil si queremos capitalizar una oración con diferentes reglas para cada palabra.

## Ver también

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial de Fish Shell en español](https://www.linuxito.com/programacion/1542-tutorial-de-fish-shell)
- [Ejemplos de código de Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_examples)