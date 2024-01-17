---
title:                "Utilizando expresiones regulares"
html_title:           "Elixir: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Las expresiones regulares son una herramienta muy útil para los programadores en Elixir. Se utilizan para buscar y manipular cadenas de texto de una forma más rápida y eficiente. Son particularmente útiles para validar entradas de usuario, buscar patrones en texto y realizar operaciones de búsqueda y reemplazo.

## Cómo:

Utilizar expresiones regulares en Elixir es muy simple. Primero debes importar el módulo "Regex" en tu código:

```Elixir
import Regex
```

Luego, puedes crear una expresión regular usando la función "compile" y pasarle un patrón entre comillas. Por ejemplo, si queremos encontrar todas las coincidencias de la palabra "hola" en una cadena de texto, podemos hacer lo siguiente:

```Elixir
regex = Regex.compile("hola")
```

Para buscar esta expresión regular en una cadena de texto, podemos utilizar la función "match?" y pasarle la cadena y la expresión regular como parámetros. Si encuentra una coincidencia, devolverá "true", de lo contrario, devolverá "false". Por ejemplo:

```Elixir
Regex.match?("¡Hola Mundo!", regex)
=> true
```

También puedes utilizar expresiones regulares en combinación con la función "replace" para reemplazar ciertos patrones en una cadena de texto. Por ejemplo, si queremos reemplazar todas las letras "a" por "e" en una cadena, podríamos hacerlo de la siguiente manera:

```Elixir
Regex.replace("hola", ~r/a/, "e")
=> "hole"
```

## Profundizando:

Las expresiones regulares han existido durante décadas y han sido utilizadas en varios lenguajes de programación. Sin embargo, en Elixir, se han integrado de una manera muy elegante y sencilla gracias a su soporte para las cadenas de texto en formato UTF-8.

Aunque las expresiones regulares son muy útiles, no son la única forma de buscar y manipular cadenas de texto en Elixir. También puedes utilizar patrones de coincidencia y funciones como "scan" y "split".

Si estás interesado en aprender más sobre las expresiones regulares en Elixir, puedes consultar la documentación oficial en el sitio web de Elixir o buscar tutoriales y ejemplos en línea.

## Véase también:

- Documentación oficial de Elixir sobre expresiones regulares: https://elixir-lang.org/getting-started/pattern-matching.html#regular-expressions
- Ejemplos de expresiones regulares en Elixir: https://hexdocs.pm/elixir/Regex.html
- Tutoriales en línea sobre el uso de expresiones regulares en Elixir: https://www.tutorialspoint.com/elixir/elixir_regular_expressions.htm