---
title:    "Elixir: Capitalización de una cadena"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena de texto en Elixir?

Capitalizar una cadena de texto puede ser útil para mejorar la legibilidad y presentación de datos en una aplicación. Además, puede ser necesario cuando se trabaja con información sensible, como nombres de usuarios o contraseñas.

## Cómo hacerlo

Utilizar la función `String.capitalize/1` es la forma más sencilla de capitalizar una cadena en Elixir. Aquí hay un ejemplo de código que capitaliza una cadena y muestra el resultado en la consola:

```Elixir 
cadena = "hola mundo"
capitalizado = String.capitalize(cadena)
IO.puts(capitalizado)
```

Salida: "Hola mundo"

También se puede utilizar `String.upcase/1` para convertir una cadena en mayúsculas o `String.downcase/1` para convertirla en minúsculas. Aquí hay un ejemplo de código que convierte una cadena a mayúsculas:

```Elixir 
cadena = "hola mundo"
mayusculas = String.upcase(cadena)
IO.puts(mayusculas)
```

Salida: "HOLA MUNDO"

## Profundizando

Elixir ofrece varias funciones para manipular y formatear cadenas de texto. Además de `String.capitalize/1`, también se pueden utilizar `String.capitalize_first/1` y `String.capitalize_words/1`. Estas funciones proporcionan más opciones para capitalizar cadenas en diferentes contextos.

Además, Elixir también ofrece el módulo `String.Case` que contiene varias funciones para manipular el formato de las cadenas de texto, incluyendo la mayoría de las funciones de capitalización.

Para obtener más información sobre cómo manipular cadenas en Elixir, se recomienda revisar la documentación oficial del lenguaje o buscar ejemplos y tutoriales en línea.

## Ver también

- [Documentación oficial de Elixir sobre cadenas de texto](https://elixir-lang.org/getting-started/string.html)
- [Ejemplos de manipulación de cadenas en Elixir](https://blog.lelonek.me/elixir-strings-in-depth-4ef7a4188aaa)
- [Tutorial de Elixir sobre cadenas y patrones de texto](https://elixirschool.com/es/lessons/basics/basics/#cadenas-y-patrones-de-texto-en-elixir)