---
title:                "Rust: Encontrar la longitud de una cadena"
simple_title:         "Encontrar la longitud de una cadena"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué
En la programación, es importante poder manipular y trabajar con cadenas de texto. Una tarea común es encontrar la longitud de una cadena, es decir, cuántos caracteres contiene. En este artículo, vamos a explorar cómo podemos hacer esto en Rust y por qué es importante.

## Cómo hacerlo
Para encontrar la longitud de una cadena en Rust, podemos usar el método `len()` en una variable que contenga una cadena. Veamos un ejemplo:

```Rust
let mi_cadena = "¡Hola mundo!";
println!("Longitud de la cadena: {}", mi_cadena.len());
```

La salida de este código sería `Longitud de la cadena: 12`, ya que la cadena contiene 12 caracteres. También podemos usar el método `len()` en una cadena directamente, sin asignarla a una variable primero:

```Rust
println!("Longitud de la cadena: {}", "¡Hola mundo!".len());
```

Esta forma es más corta y directa, pero a veces es útil tener la cadena asignada a una variable si la vamos a usar más adelante en nuestro código.

## Profundizando
En Rust, se trata de ser explícito y seguro en el manejo de tipos de datos. Por eso, cuando usamos el método `len()` en una cadena, el resultado es un número entero del tipo `usize`. Esto nos permite asegurarnos de que siempre estamos trabajando con una cantidad numérica válida, en lugar de solo confiar en la longitud de una cadena determinada por el sistema operativo.

También es importante tener en cuenta que, en Rust, cada carácter en una cadena ocupa una ubicación específica en la memoria. Por lo tanto, la longitud de una cadena en Rust no es simplemente la cantidad de caracteres, sino también el espacio en memoria que ocupa. Esto es útil en el manejo de cadenas seguras y eficientes en términos de rendimiento.

## Ver también
- [Documentación oficial de Rust sobre cadenas](https://doc.rust-lang.org/std/string/)
- [Tutorial de Rust en español](https://dev.to/erikaheidi/tutorial-de-rust-en-espanol-parte-1-342o)
- [Ejemplos prácticos de Rust](https://doc.rust-lang.org/stable/rust-by-example/)