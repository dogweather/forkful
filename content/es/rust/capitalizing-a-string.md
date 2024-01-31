---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"

category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Capitalizar un texto significa convertir la primera letra de cada palabra a mayúscula. Programadores lo hacen para normalizar datos, mejorar la legibilidad o cumplir con requerimientos estéticos o de formato.

## Cómo hacerlo:
```Rust
fn main() {
    let texto = "hola, mundo";
    let texto_capitalizado = texto.split_whitespace()
                                  .map(|palabra| {
                                      palabra.char_indices()
                                             .map(|(i, c)| if i == 0 { c.to_uppercase().collect::<String>() } else { c.to_string() })
                                             .collect::<String>()
                                  })
                                  .collect::<Vec<_>>()
                                  .join(" ");
    println!("{}", texto_capitalizado); // Imprime "Hola, Mundo"
}
```

## Análisis Detallado:
En el pasado, capitalizar texto en Rust requería escribir funciones propias o usar bibliotecas externas. Hoy, puedes capitalizar una cadena palabra por palabra usando iteradores y métodos de la `String` y el tipo `char`. Alternativas incluyen usar la biblioteca `unicase` para el manejo más complejo de casos como caracteres Unicode. Para el caso básico, la combinación de `split_whitespace`, `char_indices`, y conversión a mayúsculas con `to_uppercase` suele ser suficiente. La implementación anterior capitaliza la primera letra de cada palabra considerando espacios en blanco, pero no maneja otros caracteres no alfabéticos ni reglas específicas de localización.

## Ver También:
- [Rust std::string::String](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust Documentation on chars](https://doc.rust-lang.org/std/primitive.char.html)
