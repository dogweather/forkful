---
title:                "Gleam: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con CSV en Gleam?

CSV (Comma Separated Values) es un formato popular para almacenar y compartir datos tabulares en una estructura sencilla y legible. En Gleam, trabajar con archivos CSV puede ser útil para importar y exportar datos, realizar análisis o incluso crear informes. Además, al ser un lenguaje de programación funcional, Gleam ofrece una forma sencilla y elegante de manipular datos en formato CSV.

## Cómo trabajar con CSV en Gleam

Para leer y escribir archivos CSV en Gleam, podemos utilizar la biblioteca `gleam-csv`, que nos proporciona funciones específicas para trabajar con este tipo de archivos. Veamos un ejemplo de cómo leer un archivo CSV y mostrar su contenido en la consola:

```gleam
import gleam/csv
import gleam/stream.{file, stdout}

let file = file.open("datos.csv")
let stream = file.stream()
let result = csv.read(stream)

case result {
  Ok(rows) -> stdout.write(rows)
  Err(err) -> stdout.write_err(err)
}
```

En este ejemplo, primero importamos la biblioteca `gleam-csv` y la biblioteca `gleam/stream`, que nos permite trabajar con streams de datos. Luego, abrimos un archivo CSV con la función `file.open()` y creamos un stream a partir de él. Finalmente, utilizamos la función `csv.read()` para leer el stream y obtener una lista de filas como resultado. Si la lectura es exitosa, imprimimos las filas en la consola utilizando `stdout.write()` y si hay algún error, lo imprimimos en la consola de error con `stdout.write_err()`.

También podemos crear nuestros propios archivos CSV utilizando la función `csv.write()`. Por ejemplo, si queremos guardar una lista de nombres en un archivo CSV, podemos hacerlo de la siguiente manera:

```gleam
import gleam/csv
import gleam/stream.{file, stdout}

let names = ["Maria", "Juan", "Sofia", "Carlos"]
let file = file.create("nombres.csv")
let stream = file.stream()
csv.write(stream, names)
stdout.write("Archivo CSV creado con éxito")
```

En este ejemplo, primero importamos las bibliotecas necesarias y creamos una lista de nombres. Luego, creamos un archivo CSV vacío con la función `file.create()` y un stream a partir de él. Finalmente, utilizamos la función `csv.write()` para escribir los nombres en el stream y guardarlo en el archivo CSV. Podemos comprobar que el archivo fue creado exitosamente imprimiendo un mensaje en la consola.

## Profundizando en CSV en Gleam

La biblioteca `gleam-csv` también nos ofrece funciones avanzadas para manipular los datos en formato CSV, como la posibilidad de filtrar, mapear y ordenar filas, entre otros. Además, podemos utilizar la función `csv.parse()` para convertir los datos en formato CSV a un tipo de dato específico, como una lista de tuplas o un mapa.

Es importante tener en cuenta que, al trabajar con archivos CSV en Gleam, debemos asegurarnos de que los datos estén formateados correctamente, ya que cualquier error puede provocar que la lectura o escritura del archivo falle.

## Ver también

- Documentación de `gleam-csv`: https://gleam.run/packages/gleam-csv/latest/
- Ejemplos de uso de `gleam-csv`: https://github.com/gleam-lang/gleam-csv/tree/master/examples
- Tutorial de Gleam: https://gleam.run/book/introduction.html#what-is-gleam