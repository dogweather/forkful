---
title:    "Rust: Utilizando expresiones regulares"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Por qué usar expresiones regulares en Rust?
Las expresiones regulares son una herramienta poderosa y versátil que permite buscar y manipular patrones de texto de manera eficiente. En Rust, las expresiones regulares se pueden utilizar para validar entradas de usuario, buscar cadenas de texto en archivos o realizar transformaciones de datos.

## Cómo usar expresiones regulares en Rust
Para usar expresiones regulares en Rust, primero debemos importar el módulo `regex` de la biblioteca estándar. Luego, podemos crear un objeto de expresión regular con el patrón que deseamos buscar.

```Rust
use std::regex::Regex;

fn main() {
    let re = Regex::new(r"\d{2}/\d{2}/\d{4}").unwrap();

    let text = "Hoy es 12/05/2021";

    if re.is_match(text) {
        println!("Se encontró una fecha en el texto");
    } else {
        println!("No se encontró una fecha en el texto");
    }
}
```

En este ejemplo, estamos buscando un patrón de fecha en el texto `Hoy es 12/05/2021`. Con la ayuda de la expresión regular `\d{2}/\d{2}/\d{4}`, podemos encontrar cualquier fecha en formato "dd/mm/yyyy".

La biblioteca `regex` también ofrece métodos como `find` y `replace` para buscar y reemplazar patrones en una cadena de texto.

## Deep Dive: Más información sobre el uso de expresiones regulares en Rust
Rust utiliza el motor de expresiones regulares de la biblioteca PCRE (Perl Compatible Regular Expressions). Esto significa que podemos utilizar la misma sintaxis y funciones que usamos en Perl u otros lenguajes compatibles con PCRE.

Además, Rust también proporciona una macro `regex!` que nos permite compilar expresiones regulares en tiempo de compilación, lo que puede mejorar el rendimiento en comparación con la construcción de expresiones regulares en tiempo de ejecución.

Otra característica interesante de las expresiones regulares en Rust es el soporte para CAPTURAS, que nos permite extraer partes específicas de una cadena de texto que coinciden con un patrón determinado.

## Ver también
- [Documentación de la biblioteca `regex` en Rust](https://doc.rust-lang.org/regex/regex/index.html)
- [Tutorial de Rust: Expresiones regulares](https://www.rust-lang.org/learn/regular-expressions)
- [Librería PCRE](https://www.pcre.org/)