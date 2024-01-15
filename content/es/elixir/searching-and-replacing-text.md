---
title:                "Buscando y reemplazando texto"
html_title:           "Elixir: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué buscar y reemplazar es útil

Buscar y reemplazar texto es una tarea común en la programación, especialmente cuando se trabaja en proyectos más grandes. Esta función permite a los programadores realizar cambios rápidos y eficientes en su código, ahorrando tiempo y evitando errores manuales.

## Cómo hacerlo en Elixir

```elixir
string = "Hola, Bienvenido a Elixir"
```

Para buscar y reemplazar en Elixir, se utiliza la función `String.replace`. Esta función toma tres argumentos: la cadena en la que se realizará el reemplazo, la cadena que se busca y la cadena que se utilizará para el reemplazo. Por ejemplo, si queremos cambiar "Hola" por "Hola a todos", podemos escribir:

```elixir
string = String.replace(string, "Hola", "Hola a todos")
```

Esto modificará la variable `string` para que ahora tenga el valor "Hola a todos, Bienvenido a Elixir". También se pueden realizar búsquedas y reemplazos con expresiones regulares utilizando la función `Regex.replace`.

Para realizar cambios en un archivo, se puede utilizar la librería `File`. Por ejemplo, si queremos reemplazar todas las instancias de "Hola" en un archivo con "Hola a todos", podemos usar:

```elixir
file = File.read("archivo.txt")
new_file = String.replace(file, "Hola", "Hola a todos")
File.write("archivo.txt", new_file)
```

## Profundizando en la búsqueda y reemplazo

La función `String.replace` utiliza internamente la función `String.replace_at`. Esto significa que no sólo se pueden reemplazar cadenas, sino también caracteres individuales en una cadena.

Además, Elixir ofrece la función `String.replace_leading`, que se utiliza para reemplazar solamente la primera aparición de la cadena buscada. También existen las funciones `String.replace_last` y `String.replace_all` para reemplazar la última aparición y todas las apariciones, respectivamente.

Otra característica interesante es que tanto la cadena buscada como la cadena de reemplazo pueden ser de cualquier tipo en Elixir, no sólo cadenas. Esto significa que se pueden buscar y reemplazar no sólo texto, sino también otras estructuras de datos.

## Ver también

- Documentación de String en Elixir: https://hexdocs.pm/elixir/String.html
- Tutorial de Elixir: https://elixir-lang.org/getting-started/introduction.html