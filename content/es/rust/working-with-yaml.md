---
title:                "Trabajando con yaml"
html_title:           "Rust: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¡Qué es y Por Qué!
Trabajar con YAML es una forma de estructurar y organizar información en un archivo de texto plano utilizando una sintaxis sencilla y legible para humanos. Los programadores utilizan YAML para almacenar datos y configuraciones en sus aplicaciones, ya que es fácil de leer y escribir.

## ¿Cómo Hacerlo?
Para trabajar con YAML en Rust, podemos utilizar la biblioteca `yaml-rust` disponible en crates.io. Podemos leer y escribir en archivos YAML utilizando las funciones `serde_yaml::from_str()` y `serde_yaml::to_string()`. Aquí hay un ejemplo de cómo leer y imprimir un archivo YAML utilizando esta biblioteca:

```Rust
use std::fs::File;
use std::io::prelude::*;
use serde_yaml;

fn main() {
    let mut file = File::open("ejemplo.yml").expect("No se pudo abrir el archivo");
    let mut contenido = String::new();
    file.read_to_string(&mut contenido)
        .expect("No se pudo leer el archivo");
    
    let documento: serde_yaml::Value = serde_yaml::from_str(&contenido)
        .expect("No se pudo convertir a YAML");
    
    println!("{}", documento["nombre"]);
}
```

El archivo `ejemplo.yml` podría contener lo siguiente:

```YAML
nombre: Juan
edad: 25
trabajo: Programador
hobbies:
  - Leer
  - Viajar
  - Jugar videojuegos
```

La salida del programa sería `Juan`, ya que estamos imprimiendo el valor de la clave `nombre` del documento YAML.

## Más Detalles
YAML fue presentado en el año 2001 y su nombre significa "YAML Ain't Markup Language". Se diferencia de otros formatos de datos como JSON por su legibilidad para humanos y su capacidad de incluir comentarios y referencias a otros archivos.

Existen otras bibliotecas de Rust para trabajar con YAML, como `yaml-rust` de xi-editor y `serde-yaml` de rhsanson. Ambas ofrecen un rendimiento más rápido que `yaml-rust`, pero pueden tener algunas limitaciones en características.

La implementación de `yaml-rust` se basa en el analizador `libyaml` escrito en C, lo que le da un buen rendimiento y compatibilidad con referencia de JSON. Aunque esta biblioteca se ha mantenido de forma constante, no se ha actualizado desde 2019.

## Véase También
- [Documentación oficial de YAML](https://yaml.org/)
- [Crates.io - yaml-rust](https://crates.io/crates/yaml-rust)
- [xi-editor/yaml-rust en GitHub](https://github.com/xi-editor/yaml-rust)
- [rhsanson/rust-serde-yaml en GitHub](https://github.com/rhsanson/rust-serde-yaml)