---
title:                "Trabajando con YAML"
aliases:
- es/rust/working-with-yaml.md
date:                  2024-02-03T19:26:38.369523-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?

En la programación con Rust, trabajar con YAML (YAML Ain't Markup Language) consiste en analizar y generar datos en formato YAML, un estándar de serialización de datos amigable para el ser humano. Los programadores integran el manejo de YAML en Rust para configurar aplicaciones, gestionar ajustes o procesar estructuras de datos complejas en un formato claro y legible, aprovechando su simplicidad sobre JSON o XML para archivos de configuración e intercambio de datos.

## Cómo hacerlo:

Rust no soporta YAML en su biblioteca estándar, por lo que comúnmente utilizamos crates de terceros como `serde` (para serializar y deserializar datos) en combinación con `serde_yaml`.

Primero, añade las dependencias a tu `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

Ahora, veamos cómo deserializar una cadena YAML en una estructura Rust y serializar una estructura Rust de vuelta en una cadena YAML.

### Deserializando YAML en estructuras de Rust

Define una estructura Rust que refleje los datos que esperas en YAML. Usa atributos de Serde para la personalización si es necesario.

```rust
use serde::{Deserialize, Serialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    name: String,
    durability: i32,
    owner: Owner,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Owner {
    name: String,
    age: i32,
}

fn main() {
    let yaml_data = "
name: Shield
durability: 300
owner:
  name: Steve
  age: 25
";

    let deserialized_config: Config = serde_yaml::from_str(yaml_data).unwrap();
    println!("{:?}", deserialized_config);
}
```

La salida de muestra al ejecutar el código Rust anterior sería:

```plaintext
Config { name: "Shield", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### Serializando estructuras de Rust en YAML

Este ejemplo toma la estructura `Config` de la sección anterior y la serializa de nuevo en formato YAML.

```rust
fn main() {
    let config = Config {
        name: String::from("Axe"),
        durability: 120,
        owner: Owner {
            name: String::from("Alex"),
            age: 30,
        },
    };

    let serialized_yaml = serde_yaml::to_string(&config).unwrap();
    println!("{}", serialized_yaml);
}
```

La salida esperada será una cadena en formato YAML:

```yaml
---
name: Axe
durability: 120
owner:
  name: Alex
  age: 30
```

Estos fragmentos demuestran cómo integrar eficientemente el análisis y la generación de YAML en tus aplicaciones Rust, utilizando los populares crates `serde` y `serde_yaml`, adaptándose a estructuras de datos complejas y proporcionando configuraciones simples y legibles para el ser humano.
