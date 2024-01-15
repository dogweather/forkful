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

## ¿Por qué trabajar con YAML en Rust?

Si estás buscando una forma sencilla y legible de almacenar datos estructurados, YAML es la opción perfecta. En este artículo, te mostraré cómo trabajar con YAML en Rust de manera eficiente y fácil.

## Cómo hacerlo

Para empezar a trabajar con YAML en Rust, lo primero que necesitas es importar la librería `serde_yaml` en tu proyecto. Puedes hacerlo agregando la siguiente línea en tu archivo `Cargo.toml`:

```
[dependencies]
serde_yaml = "0.8.14"
```

Una vez que hayas importado la librería, puedes empezar a trabajar con YAML en tus programas de Rust. Por ejemplo, si quieres leer un archivo YAML llamado `datos.yaml`, puedes hacerlo de la siguiente manera:

```Rust
use serde_yaml;

let datos: serde_yaml::Value = serde_yaml::from_reader(File::open("datos.yaml")?)?;
```

En este código, estamos leyendo el archivo YAML y guardando su contenido en una variable `datos` como un objeto `serde_yaml::Value`. Ahora, puedes acceder a los datos dentro del archivo de la siguiente manera:

```Rust
println!("Nombre: {}", datos["nombre"]);
println!("Edad: {}", datos["edad"]);
```

Para escribir datos en un archivo YAML, puedes seguir un proceso similar:

```Rust
let datos = serde_yaml::to_string(&mi_struct)?;
File::create("output.yaml")?.write_all(datos.as_bytes())?;
```

En este ejemplo, estamos convirtiendo un struct en un string de YAML y luego escribiéndolo en un archivo llamado `output.yaml`.

## Profundizando

Ahora que ya sabes cómo leer y escribir archivos YAML en Rust, es importante que entiendas cómo funciona la librería `serde_yaml` y cómo puedes aprovecharla al máximo. En primer lugar, `serde_yaml` utiliza el formato `serde_json` para representar archivos YAML, lo que significa que puedes acceder a los datos dentro del objeto `serde_yaml::Value` de la misma manera que lo harías con un objeto `serde_json::Value`.

Además, `serde_yaml` incluye funciones y macros útiles para ayudarte a convertir structs de Rust en YAML y viceversa. Por ejemplo, para convertir un struct en YAML, puedes usar la función `serde_yaml::to_string()` o la macro `serde::yaml!()`. Y para convertir un objeto YAML en un struct de Rust, puedes usar la función `serde_yaml::from_reader()` o la macro `serde::yaml!()`.

En resumen, trabajar con YAML en Rust es fácil y eficiente gracias a la librería `serde_yaml`. Ya sea que estés almacenando datos de configuración o estructuras de datos complejas, YAML es una excelente opción para trabajar con ellos en tus proyectos de Rust.

## Ver también

- [Documentación de `serde_yaml`](https://docs.serde.rs/serde_yaml/index.html)
- [Ejemplos de `serde_yaml`](https://github.com/dtolnay/serde-yaml/tree/master/examples)
- [Rust Cookbook: Parsing YAML](https://rust-lang-nursery.github.io/rust-cookbook/serialization/parse_yaml.html)