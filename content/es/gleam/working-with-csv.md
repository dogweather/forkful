---
title:                "Trabajando con csv"
html_title:           "Gleam: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

# ¿Por qué trabajar con CSV?

Así que te has decidido a trabajar con archivos CSV. ¡Eso es genial! Pero puede que te preguntes ¿por qué debería usar CSV en primer lugar? Bueno, déjame decirte que hay muchas razones para hacerlo. CSV (Valores Separados por Comas) es un formato de archivo muy popular para almacenar y compartir información tabular. Es ampliamente utilizado en una variedad de industrias, como finanzas, marketing, ciencia de datos y más.

No importa qué tipo de proyecto estés trabajando, es muy probable que en algún momento necesites trabajar con datos tabulares. Y CSV es una excelente opción para hacerlo. Es simple y fácil de leer, tanto para humanos como para máquinas. Además, la mayoría de los lenguajes de programación tienen bibliotecas que facilitan trabajar con este formato. En resumen, trabajar con CSV te permitirá manejar grandes conjuntos de datos de manera efectiva y eficiente. ¡Así que adelante y sumérgete en el mundo de CSV!

# Cómo hacerlo:

En Gleam, trabajar con CSV es muy sencillo gracias a su biblioteca estándar. Digamos que tenemos un archivo CSV llamado "peliculas.csv" con la siguiente estructura:

| Título de la película | Año de lanzamiento | Director |
| --------------------- | ------------------ | ---------|
| Toy Story | 1995 | John Lasseter |
| The Shawshank Redemption | 1994 | Frank Darabont |
| Inception | 2010 | Christopher Nolan |

Podemos acceder a la información de este archivo de la siguiente manera:

```Gleam
import csv_files

// Abrir el archivo y leer su contenido
let csv = csv_files.open("peliculas.csv")

// Obtener los títulos de las columnas
let [columnas, _] = csv_files.read_cell(csv, Some('"'))

// Obtener los datos de las películas
let [_, filas] = csv_files.read_rows(csv)

// Recorrer las filas para imprimir la información
filas
  |> Vec.iter(|fila| {
    let [titulo, año, director] = Vec.to_array(fila)
    gleam_io.info("{titulo} ({año}) dirigida por {director}")
  })
```

Y el resultado sería:

```Gleam
Toy Story (1995) dirigida por John Lasseter
The Shawshank Redemption (1994) dirigida por Frank Darabont
Inception (2010) dirigida por Christopher Nolan
```

También hay muchas otras funciones útiles en la biblioteca `csv_files`, como crear archivos CSV, agregar o eliminar filas y columnas, entre otras. Puedes encontrar más información sobre estas funciones en la documentación oficial de Gleam.

# Detalles adicionales:

Hay algunas cosas importantes a tener en cuenta al trabajar con CSV. Primero, asegúrate de que tus datos estén correctamente formateados y separados por comas. Si utilizas un programa de hoja de cálculo para crear tu archivo CSV, asegúrate de guardarlo en formato "Valores separados por comas" o CSV UTF-8 (delimitado por comas). Además, ten en cuenta que las comillas y los caracteres especiales pueden afectar la lectura de tu archivo CSV, por lo que es recomendable utilizar una biblioteca para manejar estos casos.

En resumen, trabajar con CSV es una tarea relativamente sencilla pero que puede ahorrarte mucho tiempo y esfuerzo al manejar grandes cantidades de datos. Con la biblioteca estándar de Gleam, puedes acceder y manipular información de manera eficiente. Así que no dudes en incluir CSV en tu caja de herramientas de programación.

# Ver también:

- [Documentación oficial de Gleam](https://gleam.run/)
- [Biblioteca estándar de Gleam - csv_files](https://gleam.run/modules/gleam_io/latest/GleamIO.Csv_file.html)
- [Formato de archivo CSV](https://support.google.com/docs/answer/37603?hl=es)