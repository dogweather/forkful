---
title:                "Borrando caracteres que coinciden con un patrón"
html_title:           "Elixir: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Borrar caracteres que coincidan con un patrón puede ser una tarea útil al trabajar con cadenas de texto en un programa. Puede ayudar a limpiar datos o filtrar resultados específicos.

## Cómo hacerlo

La función `String.replace/4` en Elixir nos permite reemplazar caracteres que coincidan con un patrón en una cadena de texto. Veamos un ejemplo de cómo podríamos borrar todos los espacios en blanco de una cadena:

```Elixir
string = "Hola mundo!"
replaced_string = String.replace(string, " ", "")
```

El código anterior reemplazará todos los espacios en blanco en la cadena "Hola mundo!" con una cadena vacía, resultando en "Holamundo!". Podemos utilizar esta misma lógica para borrar cualquier otro carácter que coincida con un patrón.

## Profundizando

Además de la función `String.replace/4`, también podemos utilizar la función `String.trim/2` para eliminar caracteres de un determinado patrón al comienzo o al final de una cadena de texto. Por ejemplo, si queremos borrar todos los números del principio de una cadena, podríamos hacer lo siguiente:

```Elixir
string = "1234Hola"
trimmed_string = String.trim(string, "1234")
```

Esto resultaría en la cadena "Hola". Si queremos borrar todos los símbolos de puntuación al final de una cadena, podríamos utilizar la misma función pero con los símbolos de puntuación como patrón.

## Ver también

  - [Documentación de Elixir para String.replace/4](https://hexdocs.pm/elixir/String.html#replace/4)
  - [Documentación de Elixir para String.trim/2](https://hexdocs.pm/elixir/String.html#trim/2)