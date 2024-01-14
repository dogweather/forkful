---
title:    "Elixir: Eliminando caracteres que coinciden con un patrón"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

¿Por qué deberías eliminar caracteres que coincidan con un patrón?

La eliminación de caracteres que coinciden con un patrón es una herramienta útil para limpiar y manipular cadenas de texto en Elixir. Puede ser útil en situaciones en las que se necesita eliminar ciertos caracteres no deseados de una cadena de texto, como símbolos de puntuación o espacios en blanco.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Elixir, se pueden usar las funciones `String.replace/3` o `String.replace_at/3`. Ambas funciones toman como argumento una cadena de texto, un patrón a buscar y una cadena de reemplazo.

Un ejemplo de cómo usar `String.replace` para eliminar todos los caracteres que no sean letras o números de una cadena de texto sería:

```Elixir
iex> String.replace("¡Hola! ¿Cómo estás?", ~r/[^a-zA-Z0-9]/, "")
"HolaCmoests"
```

En este caso, el patrón `~r/[^a-zA-Z0-9]/` hace coincidir con todos los caracteres que no sean letras o números y los reemplaza con una cadena vacía. Como resultado, obtendremos la cadena de texto "HolaCmoests".

También es posible usar `String.replace_at` para eliminar caracteres específicos de una cadena de texto. Por ejemplo, si queremos eliminar solo el primer espacio en blanco de una cadena de texto, podemos hacerlo de la siguiente manera:

```Elixir
iex> String.replace_at("Hola mundo", 5, "")
"Holamundo"
```

En este caso, el número 5 representa la posición del espacio en blanco que queremos eliminar.

## Profundizando

Las funciones `String.replace/3` y `String.replace_at/3` son solo algunos ejemplos de cómo se pueden eliminar caracteres que coinciden con un patrón en Elixir. Hay muchas otras funciones disponibles en el módulo `String` que pueden ser útiles para diferentes casos de uso.

También se pueden combinar varias funciones para lograr resultados más complejos. Por ejemplo, si queremos eliminar todos los caracteres no alfanuméricos de una cadena de texto, podemos usar la función `String.replace/3` junto con la función `String.trim/1` para eliminar cualquier espacio en blanco adicional que pueda quedar:

```Elixir
iex> "¡Hola! ¿Cómo estás? 42". 
|> String.replace(~r/[^a-zA-Z0-9]/, "") 
|> String.trim 
"HolaCmoests42"
```

En resumen, eliminar caracteres que coinciden con un patrón en Elixir es una tarea común en el manejo de cadenas de texto y hay muchas formas de lograrlo utilizando las funciones disponibles en el módulo `String`.

## Vea también

- [La documentación oficial de Elixir sobre el módulo `String`](https://hexdocs.pm/elixir/String.html)
- [Este artículo sobre cómo manipular cadenas de texto en Elixir](https://medium.com/@aaronbertrand/using-elixir-chains-and-strings-to-fix-a-bad-response-from-a-web-service-call-1690c4816228)