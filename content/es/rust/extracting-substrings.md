---
title:                "Extracción de subcadenas"
date:                  2024-01-20T17:46:29.554901-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracción de subcadenas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Extraer subcadenas significa elegir una parte específica de un texto. Los programadores lo hacen para analizar datos, validar entradas o simplemente trabajar con fragmentos relevantes de una cadena más grande.

## Cómo Hacerlo:
```Rust
fn main() {
    let texto = "Rust es genial";
    let subcadena = &texto[5..10]; // Toma la subcadena desde el índice 5 al 9
    println!("La subcadena es: {}", subcadena);
}
```
Salida:
```
La subcadena es: es ge
```

Para extraer una subcadena desde el principio o hasta el final:
```Rust
fn main() {
    let texto = "Hola, mundo!";
    let inicio = &texto[..5]; // Desde el comienzo hasta el índice 4
    let final = &texto[7..]; // Desde el índice 7 hasta el final
    println!("Inicio: '{}', Final: '{}'", inicio, final);
}
```
Salida:
```
Inicio: 'Hola,', Final: 'mundo!'
```

## Profundización
En Rust, las cadenas son una vista sobre una secuencia de bytes UTF-8. Extraer subcadenas no es tan simple como en otros lenguajes; necesitas trabajar con límites de caracteres para evitar errores de codificación. Aunque `.slice()` es común en otros lenguajes, Rust utiliza la indicación de rango con corchetes para mantener la seguridad y claridad.

En la historia de Rust, siempre ha sido crucial manejar correctamente la memoria y la seguridad de datos. Por eso, aunque extraer subcadenas puede parecer más verborrágico comparado con otros lenguajes, es por una buena causa: evita errores comunes, como la corrupción de bytes y vulnerabilidades de seguridad.

Además, hay alternativas para situaciones más complejas, como usar las funciones `chars()` y `bytes()` para iterar a través de caracteres o bytes específicos, o el uso de bibliotecas de terceros para patrones de búsqueda más sofisticados.

## Ver También
- Documentación oficial de Rust sobre cadenas: https://doc.rust-lang.org/book/ch08-02-strings.html#slicing-strings
- Rust by Example sobre cadenas: https://doc.rust-lang.org/stable/rust-by-example/std/str.html
- Crates para el manejo avanzado de cadenas en Rust: https://crates.io/categories/string-processing