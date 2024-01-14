---
title:                "Elixir: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

 A veces, mientras estamos escribiendo código en Elixir, nos encontramos con la necesidad de eliminar caracteres que coinciden con un patrón específico. Ya sea para limpiar datos o para simplificar una cadena, la eliminación de caracteres que coinciden con un patrón puede ser una tarea muy útil en nuestra programación.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Elixir, podemos utilizar la función `String.replace/3`. Esta función toma tres argumentos: la cadena de entrada, el patrón a buscar y la cadena de reemplazo. Veamos un ejemplo de cómo usarlo:

```
Elixir: cadena = "¡Hola, Mundo!"
Elixir: String.replace(cadena, "!", "")
"Hola, Mundo"
```

En este ejemplo, utilizamos la función `String.replace/3` para eliminar el signo de exclamación de la cadena original y obtener una nueva cadena sin ese caracter.

También podemos usar expresiones regulares en lugar de una cadena de reemplazo. Por ejemplo, si queremos eliminar todas las vocales de una cadena, podemos hacer lo siguiente:

```
Elixir: cadena = "Elixir es genial!"
Elixir: String.replace(cadena, ~r/[aeiou]/, "")
"Elxr s gnl!"
```

Aquí, usamos una expresión regular en lugar de una cadena de reemplazo. La expresión regular `~r/[aeiou]/` coincide con todas las vocales en la cadena y las reemplaza por una cadena vacía.

## Profundizando

Para aquellos que deseen conocer más sobre cómo funciona `String.replace/3` en el fondo, pueden ver la documentación oficial de Elixir para obtener más detalles. Además, también pueden explorar otras funciones de la librería `String` para realizar diferentes tipos de operaciones de manipulación de cadenas.

## Ver también

- [Documentación oficial de Elixir para `String.replace/3`](https://hexdocs.pm/elixir/String.html#replace/3)
- [Otras funciones de la librería `String` en Elixir](https://hexdocs.pm/elixir/String.html#content)