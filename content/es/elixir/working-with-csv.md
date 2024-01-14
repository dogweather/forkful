---
title:                "Elixir: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Por qué trabajar con CSV en Elixir

CSV, o Comma-Separated Values, es un formato de archivo ampliamente utilizado para almacenar datos en forma de tabla. En Elixir, trabajar con este tipo de archivos puede ser extremadamente útil para tareas como importar y exportar datos, generar informes o realizar análisis de datos. Además, con la ayuda de algunas funciones y librerías, trabajar con CSV en Elixir es sencillo y eficiente.

## Cómo hacerlo

Para trabajar con CSV en Elixir, necesitamos utilizar una librería como [CSV](https://hexdocs.pm/csv/CSV.html) o [NimbleCSV](https://hexdocs.pm/nimble_csv/NimbleCSV.html). Estas librerías nos proporcionan funciones para leer, escribir y manipular archivos CSV.

Para comenzar, necesitamos importar la librería que vayamos a utilizar en nuestro archivo de código. A continuación, podemos utilizar la función `CSV.parse` para leer los datos de un archivo CSV existente. Por ejemplo:

```
Elixir
iex> CSV.parse("path/to/file.csv")
{:ok, [["Nombre", "Apellido", "Edad"], ["Juan", "Pérez", "35"], ["María", "González", "28"], ["Pedro", "López", "42"]]}
```

La función `CSV.parse` devuelve una tupla con el átomo `:ok` y una lista de listas que representan los datos del CSV.

Si queremos escribir datos en un archivo CSV, podemos utilizar la función `CSV.encode` junto con la función `File.write!`. Por ejemplo:

```
Elixir
iex> data = [["ID", "Producto", "Precio"], [1, "Camiseta", "$20"], [2, "Zapatos", "$50"], [3, "Gorra", "$15"]]
iex> csv = CSV.encode(data)
iex> File.write!("path/to/new_file.csv", csv)
:ok
```

Este código creará un nuevo archivo CSV con los datos proporcionados en la variable `data`.

## Inmersión profunda

Trabajar con CSV en Elixir también nos permite manipular los datos de forma más compleja. Podemos utilizar funciones como `CSV.filter`, `CSV.map` y `CSV.reduce` para seleccionar, modificar y resumir los datos de nuestro archivo CSV.

Además, con la librería [Ecto](https://hexdocs.pm/ecto/Ecto.html), podemos utilizar modelos para trabajar con datos CSV, lo que puede ser útil si queremos realizar operaciones de base de datos en nuestros datos.

## Ver también

- Documentación de [CSV](https://hexdocs.pm/csv/CSV.html)
- Documentación de [NimbleCSV](https://hexdocs.pm/nimble_csv/NimbleCSV.html)
- Documentación de [Ecto](https://hexdocs.pm/ecto/Ecto.html)