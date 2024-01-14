---
title:    "Elixir: Borrando caracteres que coinciden con un patrón"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Eliminar caracteres que coinciden con un patrón es una práctica útil cuando se trabaja con cadenas de texto en Elixir. Puede ser útil para limpiar entradas de usuario, manipular cadenas complejas o realizar búsquedas y reemplazos específicos.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Elixir, podemos utilizar la función `String.replace/4` que nos permite especificar un patrón de búsqueda y reemplazo. Veamos algunos ejemplos:

```Elixir
# Eliminar todas las letras 'a' de una cadena de texto
String.replace("Hola mundo", ~r/a/, "")

# Output: "Hol mundo"

# Eliminar todos los números de una cadena de texto
String.replace("123 Elixir", ~r/[0-9]/, "")

# Output: " Elixir"
```

Como podemos ver, el primer argumento de la función es la cadena de texto en la que queremos realizar la eliminación, el segundo argumento es el patrón de búsqueda que define qué caracteres queremos eliminar y el tercer argumento es el carácter o cadena con la que queremos remplazarlos, en este caso, lo dejamos en blanco para que sean eliminados.

También podemos utilizar la función `String.replace/3` para eliminar caracteres que coinciden con un patrón utilizando una función de reemplazo personalizada. Por ejemplo:

```Elixir
# Eliminar los espacios en blanco al final de cada palabra en una cadena de texto
String.replace("Hola mundo  ", ~r/\s$/, fn match -> "" end)

# Output: "Hola mundo""
```

En este ejemplo, utilizamos una expresión regular para identificar los espacios en blanco al final de cada palabra y utilizamos una función de reemplazo personalizada que simplemente devuelve una cadena vacía para eliminarlos.

## Un poco más profundo

En realidad, cuando utilizamos `String.replace/4` o `String.replace/3`, lo que estamos haciendo es utilizar el módulo de expresiones regulares de Elixir, llamado `Regex`, para buscar y reemplazar caracteres en una cadena de texto. Si estás interesado en aprender más sobre cómo funcionan las expresiones regulares en Elixir, te recomiendo que consultes la documentación oficial [aquí](https://hexdocs.pm/elixir/Regex.html).

## Ver también

- [Documentación oficial de Elixir sobre `String.replace/4`](https://hexdocs.pm/elixir/String.html#replace/4)
- [Documentación oficial de Elixir sobre `String.replace/3`](https://hexdocs.pm/elixir/String.html#replace/3)
- [Documentación oficial de Elixir sobre el módulo `Regex`](https://hexdocs.pm/elixir/Regex.html)