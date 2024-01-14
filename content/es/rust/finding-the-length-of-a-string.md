---
title:    "Rust: Encontrando la longitud de una cadena"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo necesitamos conocer la longitud de una cadena de caracteres. Ya sea para validar una entrada de usuario o para realizar operaciones específicas en una cadena, es una tarea común en muchos programas. En este artículo, aprenderemos cómo encontrar la longitud de una cadena en Rust y algunas cosas interesantes a tener en cuenta durante el proceso.

## Cómo hacerlo

Para encontrar la longitud de una cadena en Rust, podemos utilizar el método `len()` disponible para cualquier tipo de dato `String`. Veamos un ejemplo:

```Rust
let mi_cadena = "¡Hola mundo!";
let longitud = mi_cadena.len();

println!("La longitud de la cadena es {}", longitud);

// Output: La longitud de la cadena es 12
```

Como se puede ver, el método `len()` devuelve un valor de tipo `usize`, que representa el número de bytes en la cadena. Si queremos obtener la longitud en caracteres, podemos utilizar el método `chars()` y luego contar la cantidad de elementos en el iterador resultante. Por ejemplo:

```Rust
let mi_cadena = "¡Hola mundo!";
let cantidad = mi_cadena.chars().count();

println!("La cantidad de caracteres en la cadena es {}", cantidad);

// Output: La cantidad de caracteres en la cadena es 11
```

Aquí es importante tener en cuenta que el método `count()` devuelve un `usize` también, por lo que es importante tener en cuenta las limitaciones de este tipo de datos al trabajar con cadenas de longitud extrema.

## Profundizando

Es importante tener en cuenta que en Rust, las cadenas son representadas internamente como slices (`&str`) y no como arrays de caracteres (`[char]`). Esto significa que cuando trabajamos con cadenas, en realidad estamos trabajando con referencias a una secuencia de caracteres y no con los caracteres en sí mismos. Por lo tanto, al encontrar la longitud de una cadena, en realidad estamos encontrando la longitud de la referencia a esa cadena.

También es importante mencionar que debido a que las cadenas en Rust son inmutables, no podemos modificar su contenido directamente. Por lo tanto, al trabajar con cadenas mutables, es recomendable convertirlas a tipo `String` antes de realizar cualquier operación de modificación, como por ejemplo, encontrar la longitud.

## Ver también

- [Documentación oficial de Rust sobre el tipo `String`](https://doc.rust-lang.org/std/string/index.html)
- [Tutorial de Rust en español](https://www.rust-lang.org/es-ES/learn/get-started)
- [Ejemplos de código de Rust en GitHub](https://github.com/rust-lang/rust-by-example)