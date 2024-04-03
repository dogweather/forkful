---
date: 2024-01-20 17:35:45.241866-07:00
description: "Concatenar cadenas es simplemente unir dos o m\xE1s strings en uno solo.\
  \ Los programadores lo hacen para manipular texto - combinar mensajes, insertar\u2026"
lastmod: '2024-03-13T22:44:58.838516-06:00'
model: gpt-4-1106-preview
summary: "Concatenar cadenas es simplemente unir dos o m\xE1s strings en uno solo."
title: "Concatenaci\xF3n de cadenas de texto"
weight: 3
---

## Qué & Por Qué?
Concatenar cadenas es simplemente unir dos o más strings en uno solo. Los programadores lo hacen para manipular texto - combinar mensajes, insertar valores en plantillas, y construir salidas programáticamente.

## Cómo hacerlo:
Utiliza `+` o `format!` para concatenar. Aquí tienes ejemplos:

```Rust
fn main() {
    // Usando el operador +
    let cadena1 = "Hola".to_string();
    let cadena2 = "Mundo!";
    let resultado = cadena1 + " " + &cadena2;
    println!("{}", resultado); // Muestra "Hola Mundo!"

    // Usando la macro format!
    let nuevo_resultado = format!("{} {}", "Hola", "Mundo!");
    println!("{}", nuevo_resultado); // Muestra "Hola Mundo!"
}
```

## En Profundidad:
Históricamente, concatenar strings ha variado dependiendo del lenguaje. Rust es interesante porque la propiedad de las cadenas se transfiere al concatenar con `+`, forzando a que el segundo argumento sea una referencia.

Alternativas incluyen usar `push_str` para añadir a una `String` existente sin crear una nueva:

```Rust
fn main() {
    let mut cadena = "Hola".to_string();
    cadena.push_str(" Mundo!");
    println!("{}", cadena); // Muestra "Hola Mundo!"
}
```

Otra opción es el uso de macros como `write!` o `writeln!` para escribir en una `String`:

```Rust
use std::fmt::Write;

fn main() {
    let mut cadena = String::new();
    write!(&mut cadena, "{} {}", "Hola", "Mundo!").unwrap();
    println!("{}", cadena); // Muestra "Hola Mundo!"
}
```

La implementación detrás de la concatenación considera el manejo eficiente de la memoria, asegurándose de que no se haga más trabajo del necesario.

## Ver También:
- La documentación oficial sobre `String`: https://doc.rust-lang.org/std/string/struct.String.html
- Rust By Example sobre Strings: https://doc.rust-lang.org/rust-by-example/std/str.html
- The Rust Programming Language - Understanding Ownership (para aprender sobre la propiedad de variables en Rust): https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html
