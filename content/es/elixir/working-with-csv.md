---
title:                "Trabajando con archivos csv"
html_title:           "Elixir: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un desarrollador de Elixir buscando una forma sencilla de trabajar con datos en formato CSV, entonces estás en el lugar correcto. En este artículo, te explicaremos cómo puedes hacerlo utilizando las herramientas y funciones disponibles en la versión actual de Elixir.

## Cómo hacerlo

La función clave que utilizaremos para trabajar con CSV en Elixir es `File.stream!`. Esta función toma una ruta de archivo y devuelve un flujo que puede ser recorrido mediante operadores como `Enum.map` o `Stream.filter`. Veamos un ejemplo de cómo podemos utilizar `File.stream!` para leer un archivo CSV con datos de películas:

```Elixir
File.stream!("datos_peliculas.csv")
|> CSV.decode
|> Stream.map(fn [titulo, año, género, director] -> "#{titulo}, dirigida por #{director} en #{año}" end)
|> Enum.to_list
```

Y aquí está el resultado de la ejecución de este código:

```
["Titanic, dirigida por James Cameron en 1997",
 "The Shawshank Redemption, dirigida por Frank Darabont en 1994",
 "The Dark Knight, dirigida por Christopher Nolan en 2008",
 "Inception, dirigida por Christopher Nolan en 2010"]
```

Además de leer archivos CSV, también podemos utilizar `File.stream!` para escribir datos en un archivo CSV. Un ejemplo de cómo podemos hacer eso es:

```Elixir
datos = [["Avatar", 2009, "Ciencia ficción", "James Cameron"],
         ["Jaws", 1975, "Terror", "Steven Spielberg"],
         ["Casablanca", 1942, "Romance", "Michael Curtiz"]]

File.stream!("datos_peliculas.csv", [:write, :utf8])
|> CSV.encode
|> Enum.into(datos, & &1)
|> :ok
```

En este ejemplo, creamos una lista con datos de películas y luego utilizamos `Enum.into` para escribir esos datos en un archivo CSV. 

## Inmersión profunda

Además de las funciones mencionadas anteriormente, Elixir también ofrece una biblioteca llamada `Crawl` que incluye funciones adicionales para trabajar con CSV. Estas funciones incluyen la capacidad de leer y escribir archivos CSV con delimitadores personalizados, trabajar con archivos grandes de manera eficiente y trabajar con datos codificados en diferentes formatos, como ISO-8859-1.

Otra opción para trabajar con CSV en Elixir es utilizar la biblioteca `NimbleCSV`. Esta biblioteca ofrece una interfaz más elegante para trabajar con archivos CSV y también incluye funciones como la conversión de datos a tipos de Elixir o la manipulación de encabezados de archivos.

Ambas opciones pueden ser utilizadas junto con `File.stream!` para proporcionar una solución completa para trabajar con CSV en Elixir.

## Véase también

- Documentación oficial de Elixir sobre `File.stream!`: https://hexdocs.pm/elixir/File.html#stream!/3
- Documentación oficial de Elixir sobre `CSV`: https://hexdocs.pm/csv/CSV.html
- Biblioteca NimbleCSV: https://hex.pm/packages/nimble_csv
- Biblioteca Crawl: https://hex.pm/packages/crawl