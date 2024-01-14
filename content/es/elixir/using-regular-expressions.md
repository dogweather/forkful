---
title:    "Elixir: Usando expresiones regulares"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué

Las expresiones regulares son una poderosa herramienta de programación que permite buscar patrones en cadenas de texto. Esto es especialmente útil cuando se trabaja con grandes cantidades de datos o se necesita validar la información ingresada por el usuario.

## Cómo hacerlo

Para utilizar expresiones regulares en Elixir, necesitaremos importar el módulo `Regex` de la biblioteca estándar. Luego, podemos utilizar la función `match?` para verificar si una cadena de texto coincide con un patrón determinado.

```
Elixir
iex> import Regex

iex> match?("hola", ~r/^h/)
true
```

En el ejemplo anterior, utilizamos la expresión regular `^h` para verificar si la cadena comienza con la letra "h". La función `match?` devuelve `true` si hay una coincidencia y `false` si no la hay.

También podemos utilizar grupos para extraer información específica de una cadena de texto que coincida con un patrón. Por ejemplo, si queremos validar un número de teléfono en el formato xxx-xxx-xxxx, podemos hacer lo siguiente:

```
Elixir
iex> match?("111-222-3333", ~r/^(\d{3})-(\d{3})-(\d{4})$/)
true
```

En este caso, utilizamos tres grupos para capturar los tres grupos de dígitos separados por guiones.

## Profundizando

Las expresiones regulares en Elixir son compatibles con las expresiones regulares de Perl, por lo que podemos utilizar muchos de los patrones y operadores que ya conocemos.

Sin embargo, también existen algunas diferencias en la sintaxis. Por ejemplo, en lugar de utilizar `\d` para buscar un dígito, en Elixir utilizamos `[0-9]`.

Además, Elixir también proporciona una sintaxis más clara y legible para expresiones regulares más complejas, como por ejemplo:

```
Elixir
~r/^hola (mundo|amigos)$/i
```

Esta expresión regular buscará la cadena "hola" seguida de "mundo" o "amigos" (en cualquier combinación de mayúsculas y minúsculas). La barra vertical `|` funciona como el operador lógico "o" y la opción `i` después del patrón indica que la búsqueda no es sensible a mayúsculas y minúsculas.

## Véase también

- [Documentación oficial de expresiones regulares en Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Tutorial de expresiones regulares en Elixir](https://www.tutorialspoint.com/elixir/elixir_regular_expressions.htm)
- [Ejemplos de expresiones regulares en Elixir](https://gist.github.com/radar/148328)

Espero que esta breve introducción a las expresiones regulares en Elixir haya sido útil y te permita aprovechar al máximo esta herramienta poderosa en tus proyectos futuros. ¡Feliz programación!