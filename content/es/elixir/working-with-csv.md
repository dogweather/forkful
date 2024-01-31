---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"

category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Trabajar con CSV implica gestionar archivos de texto separados por comas, usualmente para importar o exportar datos tabulares. Los programadores utilizan CSV por su simplicidad y amplia compatibilidad con sistemas y aplicaciones de manejo de datos.

## Cómo hacerlo:

Para procesar archivos CSV en Elixir, empleamos la biblioteca CSV, la cual puedes instalar añadiendo `{:csv, "~> 2.4"}` a tu archivo `mix.exs`. Aquí un ejemplo de cómo leer y escribir:

```elixir
# Para leer un CSV
{:ok, data} = File.read("datos.csv")
{:ok, parsed_data} = CSV.decode(data)

# Para escribir en un CSV
CSV.encode_to_file([["nombre", "edad"], ["Alice", 30]], "salida.csv")
```

Si ejecutas el código, "salida.csv" contendrá:

```plaintext
nombre,edad
Alice,30
```

## Inmersión Profunda:

Históricamente, el formato CSV es uno de los más antiguos para intercambia datos, surgido en los primeros años de la informática como una manera sencilla de representar tablas. Hay alternativas como JSON o XML, pero CSV se mantiene por su mínimo uso de ancho de banda y facilidad de lectura para humanos y máquinas. En Elixir, trabajar con CSV puede requerir el manejo de colecciones y el módulo `Stream` para procesar archivos grandes de manera eficiente.

## Ver También:

- [`CSV` Hex package](https://hex.pm/packages/csv): Documentación oficial del paquete CSV.
- [Elixir School](https://elixirschool.com/en/): Lecciones sobre la programación en Elixir, incluyendo el manejo de archivos.
- [Elixir Forum](https://elixirforum.com/): Una comunidad donde puedes preguntar dudas o discutir sobre Elixir y CSV.
