---
title:                "Capitalizando una cadena de texto"
html_title:           "Rust: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Capitalizar una cadena implica convertir la primera letra de cada palabra en una letra mayúscula. Los programadores lo hacen para mejorar la legibilidad y hacer que las cadenas estén en un formato estándar.

## Cómo hacerlo:

```Rust
fn main() {
    let mi_cadena = "hola mundo";
    let resultado = mi_cadena.to_uppercase();
    println!("{}", resultado);
}
```

Este programa imprimirá "HOLA MUNDO".

## Inmersión profunda

Históricamente, la capitalización se ha utilizado en los lenguajes de programación para mejorar la legibilidad y estandarizar la salida de las cadenas.

Otras formas de capitalizar en Rust incluyen la función to_lowercase para convertir todas las letras a minúsculas, o usar la función char::to_uppercase en cada caracter de la cadena.

Para capitalizar una cadena en Rust, se crea una nueva cadena con las letras cambiadas a mayúsculas. Rust utiliza el concepto de propiedad de datos, por lo que la función to_uppercase devuelve una nueva cadena. Esto significa que no puede cambiar la cadena original a menos que la sobreescriba con el valor devuelto por to_uppercase.

## Ver también:

Puede encontrar más información sobre las cadenas en Rust y sus métodos asociados en los siguientes recursos:

- Documentación oficial de Rust sobre cadenas: https://doc.rust-lang.org/book/ch08-02-strings.html
- Artículo acerca de cómo trabajar con strings en Rust: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html
- Post de blog “Understanding string &str in Rust”: https://hermanradtke.com/2015/05/03/string-vs-str-in-rust-functions.html