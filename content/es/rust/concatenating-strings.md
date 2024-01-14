---
title:                "Rust: Concatenando cadenas"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

# ¡Aprende a concatenar cadenas en Rust!

## ¿Por qué concatenar cadenas?

La concatenación de cadenas es una tarea común en la programación. En Rust, se refiere a la unión de dos o más cadenas de texto para formar una cadena más larga. Esto puede ser útil para construir mensajes de error, crear URL dinámicas o simplemente para mostrar resultados en la pantalla.

## Cómo hacerlo

La sintaxis básica para concatenar cadenas en Rust es usando el operador `+`. Por ejemplo:

```Rust
let nombre = "Juan";
let apellido = "Pérez";
let nombre_completo = nombre + " " + apellido;

println!("¡Hola, {}!", nombre_completo);
```

Este código imprimirá: `¡Hola, Juan Pérez!`. Como puedes ver, el operador `+` se utiliza para unir las cadenas y se puede utilizar tantas veces como sea necesario.

Sin embargo, en Rust también existe el tipo de dato `String` que permite la mutabilidad y el almacenamiento de cadenas de texto más largas. En lugar de usar el operador `+`, para concatenar cadenas de texto más largas se puede utilizar el método `push_str()` de este tipo de dato. Por ejemplo:

```Rust
let mut mensaje = String::from("¡Hola, ");
let nombre = "María";

mensaje.push_str(nombre);
mensaje.push_str("!");

println!("{}", mensaje);
```

Este código imprimirá: `¡Hola, María!`. Como se puede ver, primero se crea una variable `String` vacía y luego se va agregando texto utilizando el método `push_str()`. Esto puede ser útil si no se conocen de antemano las cadenas que se van a concatenar.

## Profundizando en la concatenación de cadenas

En Rust, la concatenación de cadenas puede ser más compleja cuando se trata de tipos de datos diferentes. Por ejemplo, si se intenta concatenar una cadena literal (`&str`) con una variable `String`, se obtendrá un error de tipo. Esto es debido a la diferencia en sus respectivos tipos de datos.

Para solucionar esto, se puede utilizar el método `to_owned()` para convertir la cadena literal a un tipo `String` antes de concatenarlos juntos. Por ejemplo:

```Rust
let apellido = "Gómez";
let primer_nombre = String::from("Ana");
let mensaje = primer_nombre + " " + apellido.to_owned(); // Se convierte a String

println!("{}", mensaje);
```

Este código imprimirá: `Ana Gómez`. También es importante tener en cuenta que la concatenación de cadenas puede ser más lenta en Rust, ya que el compilador realiza comprobaciones de seguridad adicionales para garantizar que no se produzcan problemas de memoria.

## Ver también

- [Documentación oficial de Rust sobre la concatenación de cadenas](https://doc.rust-lang.org/std/string/struct.String.html#string-concatenation)
- [Guía rápida de Rust para principiantes](https://www.rust-lang.org/learn)
- [Ejercicios prácticos para practicar Rust](https://www.codewars.com/?language=rust)