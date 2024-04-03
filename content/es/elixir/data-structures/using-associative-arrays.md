---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:42.375616-07:00
description: "En Elixir, los arreglos asociativos, llamados Mapas, son colecciones\
  \ de pares clave-valor donde una clave \xFAnica apunta a un valor. Son extremadamente\u2026"
lastmod: '2024-03-13T22:44:58.695259-06:00'
model: gpt-4-0125-preview
summary: "En Elixir, los arreglos asociativos, llamados Mapas, son colecciones de\
  \ pares clave-valor donde una clave \xFAnica apunta a un valor."
title: Uso de matrices asociativas
weight: 15
---

## Qué y Por Qué?

En Elixir, los arreglos asociativos, llamados Mapas, son colecciones de pares clave-valor donde una clave única apunta a un valor. Son extremadamente útiles para almacenar y recuperar datos sobre la marcha, haciendo tu código más limpio y tu vida más fácil.

## Cómo hacerlo:

Crear un Mapa es sencillo. Usas la sintaxis `%{}`, así:

```elixir
my_map = %{"name" => "Alex", "age" => 32}
IO.inspect(my_map)
```

Acceder a los valores se hace usando las claves:

```elixir
IO.puts my_map["name"]
```
Salida: `Alex`

Para agregar o actualizar valores, puedes usar la función `Map.put/3`:

```elixir
updated_map = Map.put(my_map, "location", "NY")
IO.inspect(updated_map)
```
Salida: `%{"age" => 32, "location" => "NY", "name" => "Alex"}`

Eliminar claves es igual de simple con `Map.delete/2`:

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
Salida: `%{"location" => "NY", "name" => "Alex"}`

## Profundización

Los Mapas en Elixir son una evolución de antiguos tipos de almacenamiento de clave-valor, como Hashes en Ruby o Diccionarios en Python. Permiten búsquedas e inserciones más eficientes, convirtiéndolos en la opción preferida para la programación moderna con Elixir. Vale la pena mencionar que antes de los Mapas, Elixir usaba los módulos HashDict y Dict, que ahora están obsoletos.

Sin embargo, para escenarios que requieren datos ordenados, podrías mirar las listas de palabras clave en Elixir. Estas son listas de tuplas, eficientes para colecciones más pequeñas pero no tan amigables en rendimiento para grandes conjuntos de datos como los Mapas.

Ten en cuenta que los Mapas almacenan sus claves en una estructura "plana", lo que hace que el acceso directo a valores anidados sea un poco complicado. Para anidaciones profundas, podrías considerar el acceso estructurado a través de las funciones `get_in`, `put_in`, `update_in`, y `get_and_update_in`, que permiten un enfoque más dinámico para la manipulación de datos anidados.

En resumen, mientras que los Mapas son tu opción para necesidades de arreglos asociativos en Elixir, el lenguaje ofrece una rica variedad de estructuras de datos para cada escenario, alentándote a elegir la herramienta adecuada para el trabajo.
