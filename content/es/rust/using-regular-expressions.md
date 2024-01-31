---
title:                "Uso de expresiones regulares"
date:                  2024-01-19
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Las expresiones regulares son patrones usados para encontrar y manipular texto. Los programadores las usan por su potencia y flexibilidad al trabajar con cadenas de caracteres, permitiéndoles realizar búsquedas y reemplazos complejos de forma sencilla y eficiente.

## Cómo hacerlo:

Para trabajar con expresiones regulares en Rust, utiliza la librería `regex`. Aquí tienes cómo incluirla en tu proyecto y algunos ejemplos básicos:

```Rust
// Añade esto a tu Cargo.toml:
// [dependencies]
// regex = "1.5.4"

use regex::Regex;

fn main() {
    let texto = "El rápido zorro marrón salta sobre el perro perezoso";
    
    // Encontrar si el patrón coincide
    let re = Regex::new(r"rápido").unwrap();
    println!("¿Se encontró 'rápido'? {}", re.is_match(texto)); // Salida: ¿Se encontró 'rápido'? true

    // Encontrar todas las coincidencias
    let re = Regex::new(r"\b\w{5}\b").unwrap();
    re.find_iter(texto).for_each(|match_| {
        println!("Palabra encontrada: {}", match_.as_str())
    });
    // Salida:
    // Palabra encontrada: rápido
    // Palabra encontrada: zorro
    // Palabra encontrada: salta
    // Palabra encontrada: sobre
    // Palabra encontrada: perro
}

```

## Inmersión profunda:

Las expresiones regulares tienen sus raíces en la teoría de autómatas y lenguajes formales que se desarrollaron en la década de 1950. Alternativas a `regex` en Rust incluyen trabajar con métodos de strings o bibliotecas de análisis de texto especializadas. Al implementar expresiones regulares, considera la eficiencia y la complejidad que pueden añadir al código; la compilación de expresiones regulares puede ser costosa si se realiza muchas veces.

## Ver también:

- La documentación oficial de `regex` en Rust: [https://docs.rs/regex](https://docs.rs/regex)
- Un tutorial interactivo para aprender expresiones regulares: [https://regexone.com/](https://regexone.com/)
- 'The Book' de Rust sobre cómo manejar texto (incluyendo expresiones regulares): [https://doc.rust-lang.org/book/ch08-02-strings.html](https://doc.rust-lang.org/book/ch08-02-strings.html)
