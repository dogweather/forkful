---
title:                "Trabajando con json"
html_title:           "Rust: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Trabajar con JSON es una forma común para almacenar y transferir datos en la programación. JSON es un formato de texto simple que facilita la lectura y escritura de datos estructurados. Los programadores utilizan JSON porque es un formato ligero y ampliamente soportado por muchos lenguajes de programación.

## ¿Cómo hacerlo?

Para trabajar con JSON en Rust, primero debes importar la biblioteca "serde_json". Luego, puedes utilizar varias macros y funciones proporcionadas por la biblioteca para serializar y deserializar datos JSON. Aquí hay un ejemplo de cómo crear y serializar un objeto JSON:

```Rust
use serde::{Serialize, Deserialize};
use serde_json::Result;

#[derive(Serialize, Deserialize)]
struct Persona {
    nombre: String,
    edad: u32,
    ciudad: String,
}

fn main() -> Result<()> {
    let persona = Persona{
        nombre: "Juan".to_string(),
        edad: 25,
        ciudad: "Ciudad de México".to_string(),
    };

    let json = serde_json::to_string(&persona)?;

    println!("{}", json); // output: {"nombre":"Juan","edad":25,"ciudad":"Ciudad de México"}

    Ok(())
}
```

Para deserializar un objeto JSON en Rust, simplemente debes utilizar la función "serde_json::from_str" y especificar la estructura en la que deseas almacenar los datos. Aquí hay un ejemplo:

```Rust
let data = r#"
    {
        "nombre": "Ana",
        "edad": 30,
        "ciudad": "Madrid"
    }
"#;

let persona: Persona = serde_json::from_str(data)?;

println!("{}", persona.nombre); // output: Ana
```

## Inmersión Profunda

JSON (JavaScript Object Notation) fue creado originalmente por Douglas Crockford en 2001. Se inspiró en otros formatos de datos, como CSON y YAML, y lo diseñó para ser fácilmente legible y utilizable por humanos y máquinas. 

Aunque JSON es un formato muy popular y ampliamente utilizado, existen otras alternativas, como XML y YAML. Sin embargo, JSON sigue siendo una elección popular debido a su simplicidad y compatibilidad con muchos lenguajes de programación.

La implementación de la biblioteca "serde_json" se basa en el uso de "serde" (Serialization/Deserialization), una biblioteca de serialización y deserialización para Rust. Serde proporciona una forma fácil y robusta de trabajar con diferentes formatos de datos, incluyendo JSON.

## Ver También

- Documentación oficial de la biblioteca "serde_json": https://docs.serde.rs/serde_json/
- Tutorial de Rust sobre cómo trabajar con JSON: https://www.ralfj.de/projects/rust-101/main.html
- Ejemplos de código en Rust utilizando "serde_json": https://github.com/serde-rs/json/tree/master/examples