---
title:                "Rust: Extrayendo subcadenas"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

# ¿Por qué extraer substrings en Rust?

La extracción de substrings es una técnica útil en la programación que permite obtener una parte específica de una cadena de caracteres. En Rust, esta tarea puede ser realizada de manera eficiente y segura gracias a las características del lenguaje.

## Cómo hacerlo

Para extraer un substring en Rust, se puede utilizar el método `get()` de la implementación `str` en una cadena de texto. Por ejemplo:

```Rust
let texto = "Hola Mundo";
let sub = texto.get(0..4);
println!("{}", sub);
```

Este código imprimirá "Hola", ya que el método `get()` toma un rango de índices y devuelve una referencia al substring correspondiente.

Otra opción es utilizar el método `slice()` en una cadena de texto mutable, lo que permite modificar directamente el substring. Por ejemplo:

```Rust
let mut texto = String::from("¡Hola Mundo!");
texto.slice(0..2);
println!("{}", texto);
```

Este código imprimirá "¡Mundo!", ya que el substring "Hola" ha sido reemplazado por "Mundo".

## Inmersión profunda

Para aquellos interesados en aprender más sobre la extracción de substrings en Rust, existen otras opciones disponibles, como el uso de la indexación en cadenas de texto y el uso de métodos como `split()` y `splitn()`. También es importante tener en cuenta las diferencias entre `get()`, `slice()` y `substring()` en términos de eficiencia y seguridad.

# Ver también

- [Documentación oficial de Rust sobre substrings](https://doc.rust-lang.org/std/primitive.str.html#method.get)
- [Tutorial de Rust: Manipulación de Strings](https://www.rust-lang.org/es-ES/learn/basic/string-operations)