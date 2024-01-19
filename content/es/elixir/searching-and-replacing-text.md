---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

La búsqueda y reemplazo de texto son operaciones comunes en programación para analizar y modificar datos. Los programadores las usamos cuando necesitamos encontrar una cadena específica y cambiarla por otra, para limpiar o formatear información, automatizar procesos y más.

## Cómo se hacen:

El módulo `String` en Elixir proporciona funciones para buscar y reemplazar texto. Aquí te muestro un ejemplo simple:

```elixir
cadena = "Hola mundo"
IO.puts String.replace(cadena, "mundo", "México")
```

Este programa encontrará la frase "mundo" en la cadena original y la reemplazará por "México". La salida será:

```elixir
"Hola México"
```

## Análisis profundo

Elixir es conocido por su eficiencia en el manejo de cadenas, heredada de Erlang, lengua madre con un sólido historial en sistemas de telecomunicación. Además de la función `replace` que vimos, Elixir ofrece otras para buscar y modificar cadenas.

Por ejempló, `regex` es una potente herramienta para buscar patrones más complejos. Pero hay que tener cuidado, porque el uso indiscriminado de expresiones regulares puede generar código difícil de entender o mantener. 

Así que siempre piensa en el equilibrio entre simplicidad y rendimiento al seleccionar la función que usarás.

## Ver también

- La [documentación oficial de Elixir](https://elixir-lang.org/getting-started/introduction.html) proporciona una visión completa de las herramientas disponibles en el lenguaje.
- Este artículo en [elixirschool](https://elixirschool.com/es/lessons/basics/strings) es un buen recurso para entender la manipulación de cadenas en Elixir.
- El blog [ElixirOutlaws](https://elixiroutlaws.com/) ofrece análisis profundos y puntos de vista prácticos sobre muchos aspectos del lenguaje Elixir.