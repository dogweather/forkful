---
title:    "Elixir: Buscando y reemplazando texto"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué
Si eres un programador, es probable que alguna vez hayas tenido que realizar una búsqueda y reemplazo de texto en tu código. En lugar de hacerlo manualmente, Elixir ofrece una forma rápida y sencilla de hacerlo a través de su función `String.replace/3`. Aprender a utilizar esta función puede ahorrar tiempo y esfuerzo en tus proyectos de programación.

## Cómo
La función `String.replace/3` toma tres argumentos: el texto en el que deseas realizar la búsqueda y el reemplazo, la cadena a buscar y la cadena a reemplazar.

Por ejemplo, si queremos reemplazar la palabra "mundo" por "universo" en la cadena de texto "Hola mundo", podemos escribir el siguiente código en Elixir:

```Elixir
cadena = "Hola mundo"
String.replace(cadena, "mundo", "universo")
```

El resultado de este código sería la cadena de texto "Hola universo".

Otra característica interesante de `String.replace/3` es que también se puede utilizar para reemplazar varias instancias de una cadena a la vez. Por ejemplo, si queremos reemplazar todos los caracteres "a" por "A" en la cadena de texto "hola amigo", podemos hacerlo de la siguiente manera:

```Elixir
cadena = "hola amigo"
String.replace(cadena, "a", "A")
```

El resultado de este código sería la cadena de texto "holA Amigo".

## Deep Dive
Además de la función básica de `String.replace/3`, Elixir también ofrece otras funciones relacionadas que pueden ser útiles en diferentes escenarios.

Por ejemplo, si queremos reemplazar una cadena ignorando mayúsculas y minúsculas, podemos utilizar la función `String.replace_trailing/4`. Esta función tiene los mismos argumentos que `String.replace/3`, pero también incluye un cuarto argumento para especificar si deseamos ignorar o no las mayúsculas y minúsculas.

También existe `String.replace_first/4`, que funciona de manera similar a `String.replace/3`, pero solo reemplaza la primera instancia de la cadena buscada. Y para aquellos que deseen realizar reemplazos basados en expresiones regulares, Elixir ofrece la función `Regex.replace/4`.

En resumen, aprender a utilizar estas funciones de búsqueda y reemplazo de texto en Elixir puede mejorar significativamente la eficiencia y productividad en tus proyectos de programación.

## Ver también
- [Documentación de Elixir sobre la función String.replace/3](https://hexdocs.pm/elixir/String.html#replace/3)
- [Ejemplos de búsqueda y reemplazo en el blog Learning Elixir](https://learningelixir.joekain.com/learn/article/elixir/search-and-replace-elixir-list-string)
- [Tutorial de búsqueda y reemplazo en el sitio de Elixir School](https://elixirschool.com/es/lessons/specifics/more-enumerables/#string-replace)