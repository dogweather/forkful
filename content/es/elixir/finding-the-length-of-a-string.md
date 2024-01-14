---
title:    "Elixir: Encontrando la longitud de una cadena."
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué
Si eres un programador de Elixir, probablemente estés familiarizado con la necesidad de manipular cadenas de texto en tu código. Una de las tareas más comunes es encontrar la longitud o número de caracteres en una cadena. Aunque esto puede parecer una tarea sencilla, conocer la forma correcta de hacerlo puede ahorrarte tiempo y esfuerzo en el futuro.

## Cómo hacerlo
La forma más sencilla de encontrar la longitud de una cadena en Elixir es utilizando la función `length()` junto con el nombre de la cadena como argumento. Por ejemplo:

```Elixir
cadena = "Hola mundo"
length(cadena)
```

El resultado de este código sería `10` ya que la cadena tiene 10 caracteres incluyendo espacios. También puedes utilizar la función `String.length()` para obtener el mismo resultado.

## Paso a paso
Comencemos desglosando los pasos necesarios para encontrar la longitud de una cadena en Elixir:

1. Define una variable con el nombre de la cadena a la que quieres encontrar la longitud.
2. Utiliza la función `length()` o `String.length()` junto con el nombre de la cadena como argumento.
3. El resultado será el número de caracteres en la cadena.

Recuerda que en Elixir, las cadenas son codificadas en UTF-8, lo que significa que cada carácter no siempre ocupa un solo byte. Esto puede afectar la longitud de la cadena y debes tenerlo en cuenta al manipular cadenas en tu código.

## Profundizando
Si quieres profundizar en el tema y conocer más funciones relacionadas con la manipulación de cadenas en Elixir, puedes utilizar la función `String.graphemes()` para obtener una lista de todos los caracteres individuales en una cadena, incluyendo caracteres combinados como acentos y diéresis.

También puedes utilizar la función `String.slice()` para obtener una subcadena a partir de una cadena existente. Esta función toma dos parámetros, el primero es la cadena y el segundo es un rango específico de caracteres que deseas obtener.

## Ver también
- [Documentación oficial de Elixir para funciones de cadena](https://hexdocs.pm/elixir/String.html)
- [Ejemplos de codificación de cadenas en Elixir](https://gist.github.com/acontreras84/7c17d8d00535f5f03a71d3aa1c668a03)