---
title:    "Elixir: Concatenando cadenas"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una técnica esencial en la programación que permite combinar múltiples cadenas de texto en una sola. Esta habilidad es importante para construir mensajes, imprimir resultados y manipular datos de manera más eficiente en Elixir.

## Cómo hacerlo

Para concatenar cadenas en Elixir, se puede utilizar el operador `<>` que une dos cadenas juntas. Por ejemplo:

```Elixir
"¡Hola" <> " mundo!" # output: ¡Hola mundo!
```

También es posible utilizar la función `String.concat/2` que puede concatenar cualquier número de cadenas. Aquí hay un ejemplo:

```Elixir
String.concat(["Esta", " ", "es", " ", "una", " ", "frase"]) # output: Esta es una frase
```

Un aspecto importante a tener en cuenta es que la concatenación de cadenas en Elixir siempre crea una nueva cadena en lugar de modificar las existentes.

## Profundizando

Cuando se utilizan cadenas muy largas o muchas de ellas, es más eficiente utilizar la función `IO.iodata_to_binary/1` para concatenarlas. Esta función convierte una lista de cadenas en una sola cadena binaria, lo que puede ser útil para optimizar el rendimiento.

Otro método a tener en cuenta es el uso de la función `<<>>` para construir una cadena binaria utilizando binarios existentes. Por ejemplo:

```Elixir
<< "¡Hola", " mundo!" >> # output: ¡Hola mundo!
```

## Ver también

- [Documentación oficial sobre la concatenación de cadenas en Elixir](https://hexdocs.pm/elixir/String.html#concat/1)
- [Artículo sobre la eficiencia de la concatenación de cadenas en Elixir](https://medium.com/@peter_29798/how-efficient-is-concatenating-strings-in-elixir-f8f7bb576093)
- [Ejemplos de uso de IO.iodata_to_binary/1](https://hexdocs.pm/elixir/IO.html#iodata_to_binary/1)