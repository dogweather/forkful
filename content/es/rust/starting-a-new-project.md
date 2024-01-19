---
title:                "Iniciando un nuevo proyecto"
html_title:           "Bash: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Iniciar un nuevo proyecto en programación se trata de establecer el marco para un software desde cero. Los programadores hacen esto para dar vida a una idea, resolver un problema o mejorar una solución existente.

## Cómo hacerlo:

Para comenzar un nuevo proyecto en Rust, necesitas instalar primero el Administrador de Paquetes de Rust (Rustup). Asegúrate de tenerlo configurado correctamente ejecutando el siguiente comando en tu terminal:

```Rust
rustup --version
```

Una vez hecho esto, puedes utilizar `Cargo`, el administrador de paquetes de Rust, para crear tu nuevo proyecto. Implementa este comando:

```Rust
cargo new mi_proyecto
```

Esto crea un nuevo directorio llamado `mi_proyecto` con una estructura de archivos básica y un simple programa "Hola Mundo!".

```Rust
.
├── Cargo.toml
└── src
    └── main.rs
```

El archivo `main.rs` contendrá:

```Rust
fn main() {
    println!("¡Hola, mundo!");
}
```

## Inmersión Profunda:

Rust, creado por Graydon Hoare en Mozilla Research, se desarrolló con la intención de proporcionar seguridad de memoria sin la necesidad de un recolector de basura. La creación de nuevos proyectos en Rust se realiza principalmente a través de Cargo, pero también puedes utilizar otros recursos como `rustc` para iniciar tus proyectos.

Existe una alternativa a `Cargo` llamada `rustc`, pero no es recomendable para proyectos grandes ya que no ofrece una gestión de paquetes tan robusta. `Cargo` es la opción más popular porque también maneja dependencias para tu proyecto.

Cuando se crea un nuevo proyecto, se crea una estructura básica de archivos. `Cargo.toml` es tu archivo de manifiesto que contiene la metadata del proyecto y las dependencias. `src/main.rs` es donde se encuentra tu código main. La función `println!` es una macro que imprime texto en la consola.

## Ver También:

Para aprender más sobre la creación de nuevos proyectos en Rust, visita los siguientes enlaces:

- [La Documentación Oficial de Rust](https://doc.rust-lang.org/book/ch01-03-hello-cargo.html)
- [El Libro de Programación de Rust](https://doc.rust-lang.org/book/title-page.html)
- [Repositorio de Rust en GitHub](https://github.com/rust-lang/rust)
- [Cargo, el gestor de paquetes de Rust](https://doc.rust-lang.org/cargo/)