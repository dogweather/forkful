---
title:                "Uniendo cadenas de texto"
html_title:           "Elixir: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
La concatenación de cadenas es simplemente unir dos o más cadenas de texto en una sola cadena más larga. Esto es útil para combinar diferentes piezas de información para formar un mensaje más completo. Los programadores a menudo lo hacen para crear mensajes claros y concisos en sus programas.

## Cómo:
Las cadenas se pueden concatenar en Elixir utilizando el operador `<>`. Por ejemplo, si queremos crear una cadena que salude a alguien con su nombre, podríamos hacer lo siguiente:

```Elixir
nombre = "Carlos"
saludo = "¡Hola " <> nombre <> "!"
Salida: ¡Hola Carlos!
```

También es posible concatenar cadenas con variables de otros tipos de datos, como enteros o booleanos. Elixir los convertirá automáticamente a cadenas y los unirá.

```Elixir
nombre = "María"
edad = 25
presentacion = "¡Hola, soy " <> nombre <> " y tengo " <> edad <> " años!"
Salida: ¡Hola, soy María y tengo 25 años!
```

## Profundizando:
La concatenación de cadenas ha sido una técnica común en la programación desde los primeros días de las computadoras. Sin embargo, en algunos lenguajes de programación heredados, como C, puede ser un proceso engorroso que requiere la asignación de memoria para la cadena resultante y la copia de los contenidos de las cadenas individuales. Afortunadamente, Elixir maneja la concatenación de manera más eficiente detrás de escena, lo que significa que puedes usarla sin preocupaciones.

Si bien la concatenación es la forma más común de unir cadenas, también existen otras técnicas, como usar `String.join/2` para combinar elementos de una lista en una cadena separada por un delimitador. Sin embargo, en la mayoría de los casos, la concatenación sigue siendo la forma más simple y efectiva de unir cadenas en Elixir.

## Ver también:
- [Elixir docs](https://hexdocs.pm/elixir/String.html#concatenation/2)
- [Tutorial en español de Elixir](https://elixir-lang.org/getting-started/introduction.html#string-concatenation)