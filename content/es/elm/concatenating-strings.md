---
title:                "Elm: Uniendo cadenas"
simple_title:         "Uniendo cadenas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

¡Hola, programadores de Elm! Hoy hablaremos sobre cómo concatenar cadenas de texto en Elm y por qué es una habilidad importante para tener.

## ¿Por qué?

Concatenar cadenas de texto es una práctica común en la programación, especialmente cuando se trabaja con datos dinámicos. En lugar de tener una cadena de texto estática, la concatenación nos permite unir varias cadenas para crear una más larga y compleja. Esto puede ser útil en situaciones como mostrar mensajes personalizados en una aplicación, crear URLs dinámicas o construir código HTML.

## Cómo hacerlo

Para concatenar cadenas de texto en Elm, utilizamos el operador `++`. Veamos un ejemplo simple de cómo se ve esto:

```Elm
saludo = "¡Hola "
nombre = "Pedro"
mensaje = saludo ++ nombre ++ "!"
```

En este caso, `mensaje` será igual a "¡Hola Pedro!", ya que hemos unido las cadenas de texto con el operador `++`. Notarás que hemos incluido un espacio después de "Hola" para que el resultado final no quede pegado. Si no lo hiciéramos, obtendríamos "¡HolaPedro!".

## Profundizando

Mientras que el uso básico del operador `++` es bastante simple, hay algunas cosas interesantes a tener en cuenta. Por ejemplo, si una de las cadenas es una lista de caracteres (`List Char`), no podremos unirla directamente con otra cadena de texto. En su lugar, debemos usar la función `List.map` para convertirla en una lista de cadenas. Esto puede ser confuso al principio, pero es una buena oportunidad para aprender sobre tipos en Elm.

Además, es importante tener en cuenta que la concatenación consume memoria. Con cadenas de texto pequeñas no es un problema, pero si estamos manejando grandes cantidades de datos, es posible que necesitemos encontrar una solución más eficiente.

## Ver también

- Documentación oficial de Elm sobre la concatenación de cadenas: https://guide.elm-lang.org/appendix/strings.html
- Ejemplos de cómo utilizar el operador `++` en la vida real: https://legacy.gitbook.com/book/elm-tutorial/string-concatenation-in-action/details
- Artículo sobre cómo optimizar el rendimiento al concatenar cadenas: https://dev.to/benshope/elm-string-concatenation-performance-tips-33fl