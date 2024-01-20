---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Convertir una fecha en una cadena es cuando cambiamos un formato de fecha, como DD/MM/AAAA, a una cadena de texto ("2/10/2021"). Los programadores hacen esto para facilitar la manipulación de fechas y la visualización en varias interfaces de usuario.

## Cómo se hace:
En Gleam, la conversión de una fecha a una cadena de texto se hace a través del módulo `erlang`.

```gleam
import erlang

fn main() {
-> Tuple(tuple([1, 1, 1970]))
  |> erlang.tuple_to_list
  |> list.map(fn(x) { str.from_int(x) })
  |> list.append(["/"])
}
```
La salida será:

```gleam
["1", "/", "1", "/", "1970"]
```
## Buceo en profundidad
Convertir fechas en cadenas de texto es una práctica común en programación. Históricamente, esto se hizo para trabajar alrededor de limitaciones de lenguaje y biblioteca, y para ahorrar espacio de almacenamiento. Otras alternativas incluyen el uso de `date_to_string` en otras bibliotecas de terceros o lenguajes.

El módulo `erlang` en Gleam convierte las fechas en cadenas por medio de la conversión de cada componente (día, mes, año) a cadenas de texto, y luego los concatena con un "/". Este método es simple y directo, y hace que la fecha sea fácilmente legible y manipulable desde cualquier lugar del programa.

## Ver También
Para más información sobre Gleam y sus utilidades, puedes visitar los siguientes enlaces:

- Artículo útil sobre la conversión de fechas a cadenas: [https://www.toptal.com/software/definitive-guide-to-datetime-manipulation](https://www.toptal.com/software/definitive-guide-to-datetime-manipulation)