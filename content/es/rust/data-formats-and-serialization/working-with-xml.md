---
date: 2024-01-26 04:35:21.116154-07:00
description: "XML, siglas de eXtensible Markup Language (Lenguaje de Marcado Extensible),\
  \ es como el primo verborr\xE1gico de JSON. Luchar\xE1s con XML al tratar con sistemas\u2026"
lastmod: '2024-03-13T22:44:58.871024-06:00'
model: gpt-4-0125-preview
summary: "XML, siglas de eXtensible Markup Language (Lenguaje de Marcado Extensible),\
  \ es como el primo verborr\xE1gico de JSON."
title: Trabajando con XML
weight: 40
---

## Cómo hacerlo:
En Rust, puedes manejar XML con crates como `xml-rs`. Instala agregando `xml-rs = "0.8"` a tu `Cargo.toml`. Aquí te mostramos cómo analizar un XML simple:

```rust
extern crate xml;

use xml::reader::{EventReader, XmlEvent};

fn main() {
    let xml_data = r#"<book category="fiction">
    <title>Rust in Action</title>
    <author>Tim McNamara</author>
    <year>2021</year>
</book>"#;

    let parser = EventReader::from_str(xml_data);
    for e in parser {
        match e {
            Ok(XmlEvent::StartElement { name, .. }) => {
                println!("Inicio: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("Texto: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("Fin: {}", name);
            }
            Err(e) => {
                println!("Error: {}", e);
            }
            _ => {}
        }
    }
}
```

Salida:
```
Inicio: book
Inicio: title
Texto: Rust in Action
Fin: title
Inicio: author
Texto: Tim McNamara
Fin: author
Inicio: year
Texto: 2021
Fin: year
Fin: book
```
Este código lee en flujo XML, manejando elementos de inicio y fin además de datos de texto, registrando cada paso.

## Inmersión profunda:
XML es un veterano en años tecnológicos, creado para la web a finales de los 90. Su diseño promueve la legibilidad (tanto para máquinas como para humanos) y datos auto-descriptivos extensos.

¿Alternativas? Claro, JSON es el moderno preferido para las APIs web, más ligero y menos ruidoso. Mientras tanto, YAML ha ganado fans para configuraciones, con su diseño limpio. Pero XML no va a desaparecer pronto: vastas infraestructuras están construidas sobre él.

Bajo el capó, el análisis de XML de Rust se apoya en patrones de iterador, manteniendo el uso de memoria bajo y el rendimiento agudo. Encontrarás crates como `serde-xml-rs` para una experiencia más al estilo de serde, una bendición para aquellos acostumbrados a manejar JSON.

## Ver también:
Para más sobre Rust y XML:
- `serde-xml-rs` para la compatibilidad de Rust con serde: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- Documentación oficial de Rust (porque nunca está de más repasar): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
