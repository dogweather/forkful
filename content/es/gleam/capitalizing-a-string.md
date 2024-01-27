---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizar una cadena significa convertir la primera letra de la cadena en mayúscula. Los programadores lo hacen para asegurar la consistencia en la presentación de datos o para cumplir con normas estilísticas específicas.

## How to:
En Gleam, podrías hacerlo utilizando una función que manipule los caracteres de la cadena. Aquí tienes un ejemplo sencillo:

```gleam
import gleam/string

pub fn capitalize(str: String) -> String {
  string.capitalize(str)
}

fn main() {
  let my_string = "hola mundo"
  let capitalized = capitalize(my_string)
  assert capitalized == "Hola mundo"
}
```

Salida:
```
"Hola mundo"
```

## Deep Dive
En otros lenguajes de programación, capitalizar cadenas puede ser más complicado debido a su manejo de caracteres Unicode. Sin embargo, en Gleam, el módulo `string` proporciona una función `capitalize` que facilita la tarea.

Históricamente, Gleam está inspirado en lenguajes como Erlang y Rust, y al igual que sus precursores, se ha diseñado para ser robusto y concurrente. La capitalización de cadenas es una operación común en la mayoría de los lenguajes, y, aunque Gleam es relativamente nuevo, adopta esta funcionalidad comúnmente utilizada.

Una alternativa al método `string.capitalize` sería escribir una implementación personalizada donde se pueda controlar más aspectos específicos, como el tratamiento de conjuntos especiales de caracteres o reglas de localización.

En cuanto a detalles de implementación, el soporte de Gleam para Unicode en la capitalización de cadenas hace que sea más seguro y adecuado para un conjunto diverso de aplicaciones. Por debajo, utiliza la potencia de la BEAM (la máquina virtual de Erlang) para el manejo eficiente de caracteres y cadenas.

## See Also
- [Unicode Standard](http://www.unicode.org/versions/latest/)
- [Erlang's String Module](http://erlang.org/doc/man/string.html)
