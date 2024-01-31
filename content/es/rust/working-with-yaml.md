---
title:                "Trabajando con YAML"
date:                  2024-01-19
simple_title:         "Trabajando con YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Trabajar con YAML significa manejar datos en un formato fácil de leer tanto para humanos como para máquinas. Los programadores lo utilizan para configuraciones o intercambio de información porque es sencillo y ampliamente compatible.

## Cómo Hacerlo:

Instala `serde` para serialización y `serde_yaml` para trabajar con YAML:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

Creamos un tipo de dato y lo serializamos a YAML:

```rust
use serde::{Serialize, Deserialize};

#[derive(Debug, Serialize, Deserialize)]
struct Config {
    nombre: String,
    activo: bool,
    valores: Vec<u32>,
}

fn main() {
    let config = Config {
        nombre: "YAML Ejemplo".to_string(),
        activo: true,
        valores: vec![1, 2, 3],
    };

    let serialized = serde_yaml::to_string(&config).unwrap();
    println!("YAML serializado:\n{}", serialized);
}
```

Resultado:

```
YAML serializado:
---
nombre: "YAML Ejemplo"
activo: true
valores:
  - 1
  - 2
  - 3
```

Ahora, deserializamos desde una string de YAML:

```rust
fn main() {
    let data = "
nombre: 'YAML Ejemplo'
activo: true
valores:
  - 1
  - 2
  - 3
";

    let deserialized: Config = serde_yaml::from_str(data).unwrap();
    println!("{:?}", deserialized);
}
```

Resultado:

```
Config { nombre: "YAML Ejemplo", activo: true, valores: [1, 2, 3] }
```

## Profundización:

YAML, que significa "YAML Ain't Markup Language", fue presentado en 2001. Es una alternativa a XML y JSON, buscando mayor legibilidad. En Rust, `serde_yaml` es comúnmente usado pero existen alternativas como `yaml-rust`. La implementación requiere entender el concepto de serialización y el uso de traits en Rust para proporcionar o derivar implementaciones de serialización para estructuras personalizadas.

## Ver También:

- Documentación oficial de `serde`: https://serde.rs/
- Referencia de Serde YAML: https://docs.rs/serde_yaml/latest/serde_yaml/
- ¿Qué es YAML? Conceptos y sintaxis: https://yaml.org/spec/1.2/spec.html
- Tutorial de Rust Serde: https://serde.rs/
