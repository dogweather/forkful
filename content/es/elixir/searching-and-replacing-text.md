---
title:                "Elixir: Búsqueda y reemplazo de texto"
simple_title:         "Búsqueda y reemplazo de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has necesitado realizar cambios en grandes cantidades de texto? ¿Te imaginas tener que hacerlo manualmente? ¡Eso sería una tarea tediosa y extremadamente demorada! Afortunadamente, en Elixir contamos con una función para buscar y reemplazar texto de manera eficiente. En esta publicación, aprenderemos cómo usarla para ahorrar tiempo y esfuerzo en nuestras tareas de programación.

## Cómo hacerlo

Para utilizar la función de búsqueda y reemplazo en Elixir, primero debemos importar el módulo `String` en nuestro código. Luego, podemos llamar a la función `replace/3` indicando el texto que queremos modificar, la cadena que queremos buscar y la cadena con la que queremos reemplazarla. Por ejemplo:

```Elixir
String.replace("Hola, mundo", "mundo", "amigos")

# Salida: "Hola, amigos"
```

También podemos usar expresiones regulares para realizar búsquedas más complejas. Por ejemplo, podemos reemplazar todas las vocales de una palabra con una "x" utilizando una expresión regular:

```Elixir
String.replace("Elixir", ~r/[aeiou]/, "x")

# Salida: "Exlxrx"
```

Incluso podemos usar una función de reemplazo en lugar de una cadena estática, lo que nos permite realizar cambios dinámicos en nuestro texto. Por ejemplo, podemos convertir todas las letras a mayúsculas:

```Elixir
String.replace("hola", ~r/[a-z]/, &String.upcase(&1))

# Salida: "HOLA"
```

## Profundizando

Además de la función `replace/3`, Elixir también nos ofrece otras opciones para realizar búsquedas y reemplazos de texto. Por ejemplo, la función `replace/4` nos permite especificar cuántas ocurrencias de la cadena queremos reemplazar.

También podemos utilizar la función `replace_first/3` para reemplazar solo la primera ocurrencia de una cadena en lugar de todas. Y si necesitamos una opción más avanzada, la función `replace/5` nos permite especificar un patrón de búsqueda y un bloque de código que se ejecutará para cada ocurrencia encontrada.

## Ver también

- Documentación oficial de Elixir sobre funciones de reemplazo de texto: https://hexdocs.pm/elixir/String.html#replace/3
- Tutorial de Elixir sobre expresiones regulares: https://elixirschool.com/es/lessons/basics/pattern-matching/
- Ejemplos prácticos en línea de búsquedas y reemplazos en Elixir: https://kodestat.gitlab.io/post/054-string-replacement-in-elixir/