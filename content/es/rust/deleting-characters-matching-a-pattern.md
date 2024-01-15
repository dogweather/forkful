---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Rust: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué borrar caracteres que coincidan con un patrón en Rust?

Borrar caracteres que coincidan con un patrón es una tarea común en la programación. Puede ser útil para limpiar datos, validar entradas del usuario o manipular cadenas de caracteres. En Rust, hay varias formas de realizar esta tarea, dependiendo de las necesidades específicas del código.

## Cómo hacerlo

Para eliminar caracteres que coincidan con un patrón en Rust, se puede utilizar la función `replace()` de la librería estándar, que reemplaza todas las ocurrencias de un patrón en una cadena de caracteres por otro valor. Aquí hay un ejemplo de código que elimina todos los espacios en blanco de una cadena:

```Rust
let string = String::from("Hola mundo!");
let new_string = string.replace(" ", ""); 

println!("{}", new_string); // Output: "Holamundo!"
```

En este ejemplo, se crea una variable `string` con una cadena de caracteres y se utiliza la función `replace()` para reemplazar todas las ocurrencias de un espacio en blanco por una cadena vacía, lo que tiene como resultado una nueva cadena sin espacios. 

Otra forma de borrar caracteres que coincidan con un patrón en Rust es utilizando expresiones regulares a través de la librería `regex`. Las expresiones regulares son un lenguaje de patrones que se utilizan para buscar y manipular datos en cadenas de caracteres. Aquí hay un ejemplo de código que utiliza una expresión regular para eliminar todas las vocales de una cadena:

```Rust
use regex::Regex;

let string = String::from("Hola mundo!");
let re = Regex::new(r"[aeiou]").unwrap();
let new_string = re.replace_all(&string, ""); 

println!("{}", new_string); // Output: "Hl mnd!"
```

En este ejemplo, primero se importa la librería `regex` y se crea una instancia de `Regex` con un patrón que busca todas las vocales. Luego, se utiliza la función `replace_all()` para reemplazar todas las ocurrencias de dicho patrón por una cadena vacía, lo que resulta en una nueva cadena sin vocales.

## Profundizando

En Rust, también es posible utilizar el método `chars()` de una cadena de caracteres para iterar sobre cada uno de sus caracteres y eliminar aquellos que coincidan con un patrón determinado. Este enfoque puede ser útil si se requiere un control más preciso sobre los caracteres a eliminar. A continuación, se muestra un ejemplo de código que utiliza este método para borrar todas las letras mayúsculas de una cadena:

```Rust
let string = String::from("Hola Mundo!");
let mut new_string = String::new();

for c in string.chars() {
    if c.is_lowercase() {
        new_string.push(c);
    }
}

println!("{}", new_string); // Output: "ola undo!"
```

En este ejemplo, primero se crea una cadena de caracteres y se declara una variable mutable `new_string`, que se utilizará para almacenar la nueva cadena sin las letras mayúsculas. Luego, se itera sobre cada uno de los caracteres de la cadena original utilizando el método `chars()` y se verifica si es una letra minúscula a través del método `is_lowercase()`. Si es así, se agrega el caracter a `new_string` utilizando la función `push()`. Al final, se imprime la nueva cadena resultante.

## Ver también 

- Documentación oficial de Rust: https://www.rust-lang.org/es
- Librería estándar de Rust: https://doc.rust-lang.org/std/index.html
- Documentación de la librería `regex`: https://docs.rs/regex/