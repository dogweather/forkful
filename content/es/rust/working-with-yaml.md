---
title:                "Rust: Trabajando con YAML"
simple_title:         "Trabajando con YAML"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

# ¿Por qué trabajar con YAML en Rust?

YAML es un formato de serialización de datos bastante popular debido a su legibilidad y flexibilidad. Puede ser utilizado para almacenar configuraciones, datos estructurados e incluso como lenguaje de intercambio de información. Rust es un lenguaje de programación que se está haciendo cada vez más popular debido a su enfoque en la seguridad y el rendimiento. Entonces, ¿por qué no aprovechar las ventajas de ambos y trabajar con YAML en Rust?

# Cómo hacerlo

Para trabajar con YAML en Rust, primero debemos agregar la dependencia correspondiente en nuestro archivo `Cargo.toml`:

```Rust
[dependencies]
serde_yaml = "0.8.23"
```

Luego, podemos utilizar el siguiente código como ejemplo para cargar y leer un archivo YAML:

```Rust
use serde::Deserialize;
use std::fs::File;
use std::io::prelude::*;

#[derive(Debug, Deserialize)]
struct Person {
    name: String,
    age: u8,
    languages: Vec<String>,
}

fn main() {
    // Abrir archivo YAML
    let mut file = File::open("person.yml").expect("No se pudo abrir el archivo");

    // Leer el contenido
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("No se pudo leer el archivo");

    // Deserializar YAML a una estructura de datos
    let person: Person = serde_yaml::from_str(&contents).expect("No se pudo deserializar el YAML");

    // Acceder a los datos
    println!("Nombre: {}", person.name);
    println!("Edad: {}", person.age);
    println!("Lenguajes: {:?}", person.languages);
}
```

Si el contenido del archivo YAML es el siguiente:

```YAML
name: John Doe
age: 25
languages:
  - Rust
  - JavaScript
  - Python
```

La salida esperada sería:

```
Nombre: John Doe
Edad: 25
Lenguajes: ["Rust", "JavaScript", "Python"]
```

# Profundizando

Ahora que sabemos cómo leer y deserializar archivos YAML en Rust, podemos explorar más sobre cómo trabajar con ellos. La biblioteca `serde_yaml` proporciona una forma conveniente de serializar estructuras de datos en YAML. Podemos utilizar el siguiente código como ejemplo:

```Rust
use serde::{Serialize, Deserialize};
use std::fs::File;
use std::io::prelude::*;

#[derive(Debug, Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
    languages: Vec<String>,
}

fn main() {
    // Crear una instancia de la estructura
    let person = Person {
        name: "Jane Doe".to_string(),
        age: 30,
        languages: vec!["Java".to_string(), "Ruby".to_string(), "C++".to_string()],
    };

    // Serializar la estructura a YAML y escribir en un archivo
    let yaml = serde_yaml::to_string(&person).expect("No se pudo serializar la estructura");
    let mut file = File::create("person.yml").expect("No se pudo crear el archivo");
    file.write_all(&yaml.as_bytes()).expect("No se pudo escribir en el archivo");
}
```

Esto creará un archivo YAML con el siguiente contenido:

```YAML
name: Jane Doe
age: 30
languages:
  - Java
  - Ruby
  - C++
```

# Ver también

A continuación, se presentan algunos enlaces útiles para seguir aprendiendo sobre trabajar con YAML en Rust:

- [Documentación oficial de serde_yaml](https://docs.rs/serde_yaml)
- [Documentación oficial de Rust](https://www.rust-lang.org/es-ES/learn)
- [Yaml-rust: una biblioteca YAML alternativa para Rust](https://github.com/chyh1990/yaml-rust)