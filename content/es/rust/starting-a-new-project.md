---
title:                "Rust: Comenzando un nuevo proyecto"
programming_language: "Rust"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Por qué empezar un nuevo proyecto con Rust
Rust es un lenguaje de programación moderno y eficiente que se ha vuelto muy popular en los últimos años. Si estás buscando iniciar un nuevo proyecto, Rust es definitivamente una opción que debes considerar. En este artículo, te explicaremos por qué.

## Cómo empezar un proyecto en Rust
Para comenzar un nuevo proyecto en Rust, primero necesitas instalar el compilador de Rust y el gestor de paquetes Cargo. Puedes encontrar instrucciones detalladas para hacer esto en la [documentación oficial de Rust](https://www.rust-lang.org/tools/install). Una vez que tengas todo instalado, puedes seguir estos pasos para crear y ejecutar tu primer programa en Rust:

```
Rust
fn main() {
    println!("¡Hola mundo!");
}
```

Esta es una función simple que imprimirá "¡Hola mundo!" en la pantalla cuando se ejecute. Guarda este código en un archivo con la extensión `.rs`, por ejemplo, `hola_mundo.rs`. Luego, en tu terminal, navega hasta el directorio donde guardaste el archivo y ejecuta el siguiente comando:

```
Rust
$ rustc hola_mundo.rs
```

Esto compilará tu código y creará un ejecutable que puedes correr con:

```
Rust
$ ./hola_mundo
```

¡Y ahí lo tienes, tu primer programa en Rust! Puedes seguir explorando más ejemplos en la [documentación oficial](https://doc.rust-lang.org/stable/rust-by-example/) para aprender más sobre la sintaxis y las características de Rust.

## Más sobre empezar un nuevo proyecto
Una de las mejores cosas de Rust es su comunidad activa y amigable. Si tienes alguna pregunta o problema mientras empiezas tu proyecto, siempre puedes acudir a la [comunidad de Rust](https://www.rust-lang.org/community) para recibir ayuda y orientación.

Además, Rust cuenta con una gran cantidad de librerías y herramientas disponibles a través de su gestor de paquetes Cargo. Puedes explorarlas en [crates.io](https://crates.io/) para encontrar la mejor opción para tu proyecto y ahorrar tiempo en la implementación de funciones comunes.

## Ver también
- [Documentación de Rust](https://doc.rust-lang.org)
- [Crates.io](https://crates.io/)
- [Comunidad de Rust](https://www.rust-lang.org/community)