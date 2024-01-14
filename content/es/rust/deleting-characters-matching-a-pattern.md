---
title:                "Rust: Borrando caracteres que coinciden con un patrón"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

La eliminación de caracteres que coinciden con un patrón es una tarea común en la programación. Ya sea que estés trabajando con texto o datos, a veces es necesario eliminar ciertos caracteres que cumplan con ciertas condiciones. En este blog post, te mostraré cómo puedes hacerlo de manera eficiente en Rust.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón, podemos utilizar el método `retain` en un `String` o un `Vec<char>`. Este método toma una función de cierre como argumento, que se ejecuta para cada caracter en la cadena o vector. Si la función de cierre devuelve `true`, el caracter se mantiene, de lo contrario, se elimina.

Veamos un ejemplo de cómo podemos utilizar este método:

```Rust
let mut my_string = String::from("Hola mundo!");

my_string.retain(|c| c != 'a' && c != '!' && c != ' ');

println!("{}", my_string);

// Output: Holmundo
```

En este ejemplo, utilizamos el método `retain` para eliminar todos los caracteres que no sean letras, exclamaciones o espacios. Esto nos deja con la cadena "Holmundo".

También podemos usar expresiones regulares para definir nuestro patrón de caracteres a eliminar. Por ejemplo, si queremos eliminar todos los dígitos de una cadena, podemos hacer lo siguiente:

```Rust
use regex::Regex;

let re = Regex::new(r"\d").unwrap();

let mut my_string = String::from("123Hola456mundo789!");

my_string.retain(|c| !re.is_match(&c.to_string()));

println!("{}", my_string);

// Output: Holamundo!
```

Aquí, primero importamos la biblioteca `regex` y creamos un objeto de expresión regular que coincide con cualquier dígito. Luego utilizamos este objeto en la función de cierre del método `retain` para eliminar todos los dígitos de la cadena.

## Profundizando

La eliminación de caracteres que coinciden con un patrón también se puede lograr utilizando herramientas como iteradores y filtros en Rust. Sin embargo, el método `retain` es una opción más eficiente, ya que evita crear una nueva cadena o vector y en su lugar, modifica directamente el original.

También es importante tener en cuenta que el método `retain` no solo se limita a eliminar caracteres, sino que también se puede utilizar para filtrar elementos en vectores.

## Ver también

- [Documentación oficial de Rust sobre el método retain](https://doc.rust-lang.org/std/primitive.str.html#method.retain)

- [Tutorial sobre expresiones regulares en Rust](https://www.adamcodes.io/blog/lets-learn-rust-regular-expressions)