---
title:                "Trabajando con TOML"
aliases:
- /es/rust/working-with-toml.md
date:                  2024-01-26T04:25:53.837180-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/working-with-toml.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
TOML es un lenguaje de serialización de datos legible por humanos, a menudo utilizado para configuraciones. Los programadores usan TOML por su simplicidad y claridad, traduciéndose fácilmente a un mapa hash en Rust.

## Cómo hacerlo:
```Rust
// 1. Incluir la crate 'toml' en tu Cargo.toml
// [dependencies]
// toml = "0.5"

// 2. Deserializar TOML en una estructura en Rust
use toml::Value;

fn main() {
    let contenido_toml = r#"
        [server]
        host = "localhost"
        port = 8080
    "#;

    let valor = contenido_toml.parse::<Value>().unwrap();
    let host = valor.get("server").unwrap().get("host").unwrap();
    let port = valor.get("server").unwrap().get("port").unwrap();
    
    println!("El servidor está corriendo en {}:{}", host, port);
    // Salida: El servidor está corriendo en "localhost":8080
}
```

## Profundización
TOML, que significa Lenguaje Mínimo y Obvio de Tom, fue creado por Tom Preston-Werner en 2013. Su objetivo es ser más legible que JSON o YAML para archivos de configuración. El diseño de TOML se enfoca en una sintaxis no ambigua, minimalismo y mapeo sencillo a tipos de datos.

Alternativas a TOML incluyen JSON, YAML y XML, pero TOML gana en escenarios donde la legibilidad humana y la edición de archivos por no programadores es crucial. Al trabajar con TOML en Rust, serde proporciona una base sólida para la serialización y deserialización, usando rasgos para mapear TOML en las estructuras de Rust sin esfuerzo.

Un desafío al trabajar con TOML es su estricta tipificación y estructura. El programador debe definir un sistema de tipos Rust bien estructurado que refleje el esquema de los datos TOML para utilizar efectivamente TOML en Rust.

## Ver también
- [Documentación de TOML](https://toml.io/en/)
- [Crate serde_toml](https://docs.rs/serde_toml/)
- [Libro del Lenguaje de Programación Rust](https://doc.rust-lang.org/stable/book/)
- [Repositorio de TOML en GitHub](https://github.com/toml-lang/toml)
