---
title:    "Elixir: Extrayendo subcadenas"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una habilidad importante en la programación de Elixir ya que te permite manipular y procesar cadenas de texto de manera efectiva. Con esta técnica, puedes obtener solo la información relevante de una cadena y utilizarla para diversos fines, como búsquedas y validaciones.

## Cómo hacerlo

Para extraer una subcadena en Elixir, puedes utilizar la función `String.slice/3`. Esta función toma tres argumentos: la cadena original, el índice de inicio y el índice de fin. Por ejemplo:

```
Elixir> String.slice("Hola mundo", 1, 4)
"Hola"
```

En este ejemplo, la subcadena extraída comienza en el índice 1 y termina en el índice 4 (sin incluirlo). También puedes utilizar números negativos para comenzar desde el final de la cadena. Por ejemplo:

```
Elixir> String.slice("Hola mundo", -5, -1)
"mundo"
```

También puedes utilizar `String.codepoints/1` para obtener los códigos numéricos de caracteres individuales en una cadena y buscar una subcadena con `String.match?/2`. Por ejemplo:

```
Elixir> cadena = "Hola Mundo"
"Hola Mundo"

Elixir> código = String.codepoints(cadena)
[72, 111, 108, 97, 32, 77, 117, 110, 100, 111]

Elixir> String.match?(código, [111, 108, 97])
true
```

## Profundizando

Hay varias funciones y métodos en Elixir que puedes utilizar para extraer subcadenas, como `String.split/3`, `String.replace/4` y `String.trim/2`. Además, puedes utilizar patrones de expresiones regulares con `Regex.scan/2` para obtener subcadenas que coincidan con ciertos patrones. Por ejemplo:

```
Elixir> cadena = "Hoy es un día soleado"
"Hoy es un día soleado"

Elixir> Regex.scan(~r/d[a|í]a/, cadena)
[["día"]]
```

Utilizar estas funciones y métodos te permite manipular y procesar cadenas de manera más eficiente y elegante, mejorando el rendimiento de tus aplicaciones.

## Ver también

- Documentación oficial de Elixir sobre extracción de subcadenas: https://hexdocs.pm/elixir/String.html#slice/3
- Artículo sobre patrones de expresiones regulares en Elixir: https://medium.com/elixir-explained/regular-expressions-in-elixir-e256ac42ce4d
- Ejemplos prácticos de extracción de subcadenas en Elixir: https://dev.to/viniciusnegrisoli/working-with-strings-in-elixir-1nck