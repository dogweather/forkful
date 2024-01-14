---
title:                "Elixir: Borrando caracteres que coinciden con un patrón"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué eliminar caracteres que coinciden con un patrón?

Eliminar caracteres que coinciden con un patrón es una tarea común en la programación. Puede ser útil para limpiar y formatear datos, o para filtrar resultados de búsqueda. Con Elixir, esta tarea se puede realizar de manera eficiente y elegante.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Elixir, se utiliza la función `String.replace/3` junto con una expresión regular. Por ejemplo, si queremos eliminar todos los números de una cadena, podríamos usar la siguiente expresión:

```
Elixir
Input: "Hola123 Mundo456"
String.replace("Hola123 Mundo456", ~r/[0-9]/, "")
Output: "Hola Mundo"
```

En este ejemplo, usamos la expresión regular `~r/[0-9]/` la cual coincide con cualquier número en la cadena. Luego, en la función `String.replace/3` especificamos que queremos reemplazar esos caracteres con una cadena vacía `""`. Esto resulta en una cadena final sin números.

Otra forma de eliminar caracteres que coinciden con un patrón es utilizando la función `Regex.replace/3`. Esta función también utiliza expresiones regulares, pero en lugar de trabajar con cadenas, trabaja directamente con patrones. Siguiendo el mismo ejemplo anterior, podríamos escribirlo así:

```
Elixir
Input: "Hola123 Mundo456"
Regex.replace(~r/[0-9]/, "Hola123 Mundo456", "")
Output: "Hola Mundo"
```

Ambas funciones tienen sus propias particularidades y pueden ser utilizadas según la situación y la preferencia del desarrollador.

## Profundizando

Elixir tiene una librería incorporada llamada `Regex`, que nos permite crear y trabajar con expresiones regulares de manera sencilla. Esta librería proporciona una amplia gama de funciones para trabajar con patrones, incluyendo `Regex.match?`, `Regex.scan` y `Regex.split`, que pueden ser útiles en diferentes escenarios.

También es importante mencionar que en Elixir, las cadenas de caracteres son listas de caracteres, lo que significa que podemos utilizar funciones de listas, como `Enum.filter/2`, para filtrar y eliminar caracteres que coincidan con un patrón específico.

## Ver también

- [Documentación de String.replace/3](https://hexdocs.pm/elixir/String.html#replace/3)
- [Documentación de Regex.replace/3](https://hexdocs.pm/elixir/Regex.html#replace/3)
- [Documentación de la librería Regex en Elixir](https://hexdocs.pm/elixir/Regex.html)