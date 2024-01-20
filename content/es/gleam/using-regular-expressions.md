---
title:                "Uso de expresiones regulares"
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Las expresiones regulares son patrones usados para encontrar coincidencias y manipular texto. Los programadores las utilizan para validar, buscar, o reemplazar texto de manera eficiente.

## Cómo hacerlo:
Ejemplo: buscar todos los números en una cadena de texto.

```gleam
import gleam/regex

fn main() {
  let re = regex.from_str(r"\d+").unwrap()
  let text = "Los números son 123 y 456."
  let matches = regex.find_all(re, text)
  
  case matches {
    Ok(vals) -> vals |> io.debug // Muestra ["123", "456"]
    Error(_) -> io.debug("No se encontraron coincidencias")
  }
}
```

## Profundizando
Históricamente, las expresiones regulares provienen de la teoría de autómatas y lenguajes formales. Existen alternativas como el parseo estructurado o las librerías específicas para hacer coincidir patrones. En Gleam, el módulo `gleam/regex` implementa estos patrones ofreciendo flexibilidad y potencia para trabajar con texto.

## Ver También
- [Documentación oficial de Gleam](https://gleam.run/)
- [Expresiones regulares en la programación](https://www.regular-expressions.info/)