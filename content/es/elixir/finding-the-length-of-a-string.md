---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

Encontrar la longitud de una cadena, en programación, significa calcular cuántos caracteres contiene esa cadena. Los programadores a menudo lo hacen para validar la entrada del usuario, restringir la longitud del texto, o mientras procesan el texto para el análisis.

## ¿Cómo se hace?

Vamos a usar la función `String.length/1` en Elixir para encontrar la longitud de una cadena. Miramos un ejemplo:

```Elixir
IO.puts String.length("Hola Mundo")
```

La salida sería

```Elixir
10
```

En este caso, "Hola Mundo" contiene 10 caracteres, incluyendo el espacio.

## Más a Fondo

Aunque en Elixir el uso de `String.length/1` es bastante directo, vale la pena entender su implementación y detalles importantes. 

1. **Contexto histórico**: Elixir, siendo un lenguaje joven (comparado con Python o Java), aprovecha la eficacia de Erlang en el manejo de cadenas a través de listas ligadas. Sin embargo, calcular la longitud de una cadena en Elixir es una operación de O(n). Esto quiere decir que, a medida que la cadena se hace más larga, el tiempo de cálculo también aumenta.

2. **Alternativas**: Puedes usar `byte_size/1` en lugar de `String.length/1` cuando trabajas con cadenas UTF-8. Pero ten cuidado, `byte_size/1` regresa el número de bytes, no el número de caracteres.

```Elixir
IO.puts byte_size("Hola Mundo")
```

La salida sería

```Elixir
10
```

3. **Detalles de la implementación**: `String.length/1` procesa la cadena de izquierda a derecha, mientras que `byte_size/1` solo necesita consultar el descriptor de la cadena.

## Para aprender más

Para más profundidad en el trabajo con cadenas y Unicode en Elixir, consulta los siguientes recursos:

- [Elixir String Module](https://hexdocs.pm/elixir/String.html) (inglés)
- Artículo sobre [Unicode y UTF-8](https://www.joelonsoftware.com/2003/10/08/the-absolute-minimum-every-software-developer-absolutely-positively-must-know-about-unicode-and-character-sets-no-excuses/) por Joel Spolsky (inglés)