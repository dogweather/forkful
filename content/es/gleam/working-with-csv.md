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

## ¿Qué y por qué?
Trabajar con archivos CSV es una tarea común para los programadores. Estos archivos son utilizados para almacenar y manipular datos tabulares, como listas de clientes o registros de ventas. Al ser un formato sencillo y ampliamente compatible, CSV se ha convertido en la opción preferida para compartir datos entre diferentes sistemas y aplicaciones.

## Cómo hacerlo:
Para trabajar con archivos CSV en Gleam, podemos hacer uso del módulo ```gleam/csv```. Este módulo nos ofrece funciones para leer y escribir archivos CSV, así como para manipular y procesar los datos en ellos. A continuación, un ejemplo de cómo leer un archivo CSV y mostrar su contenido:

```
let archivo = csv.from_file("datos.csv")
let filas = archivo
|> csv.rows
|> List.map(fn (fila) -> fila.(0) end)

println(filas)
```

En este ejemplo, utilizamos la función ```from_file``` para cargar en la variable "archivo" un archivo CSV llamado "datos.csv". Luego, utilizamos la función ```rows``` para obtener una lista de filas del archivo y, finalmente, mediante la función ```map``` convertimos cada fila en un string y lo mostramos en la consola con la función ```println```.

## Profundizando:
Los archivos CSV han existido desde hace décadas y han sido utilizados ampliamente en diferentes áreas, como la informática y las finanzas. Aunque son una opción popular, no son la única forma de almacenar datos tabulares. Otros formatos, como JSON y XML, también pueden cumplir la misma función, pero cada uno con sus ventajas y desventajas.

En cuanto a la implementación en Gleam, el módulo ```gleam/csv``` se ha desarrollado para ser simple y fácil de usar, pero también cuenta con opciones avanzadas para personalizar su funcionamiento según nuestras necesidades. Esto lo hace una herramienta versátil y eficaz para trabajar con archivos CSV en nuestro código.

## Ver también:
Si estás interesado en aprender más sobre el trabajo con archivos CSV en Gleam, puedes revisar la documentación oficial del módulo ```gleam/csv``` en el sitio web de Gleam. También puedes consultar otros recursos en línea que ofrecen ejemplos y explicaciones detalladas sobre cómo utilizar este formato de datos en diferentes situaciones. ¡Anímate a probar y explorar todo lo que Gleam tiene para ofrecer en el manejo de archivos CSV!