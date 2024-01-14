---
title:    "Rust: Borrando caracteres que coinciden con un patrón"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Eliminar caracteres que coincidan con un patrón puede ser útil cuando se trabaja con cadenas de texto y se necesita filtrar o limpiar ciertos caracteres no deseados. Por ejemplo, al procesar datos de un archivo CSV, puede ser necesario eliminar comas o espacios en blanco para asegurarse de que los datos se lean correctamente.

## Cómo hacerlo

La librería estándar de Rust ofrece una función llamada `replace` que nos permite reemplazar cualquier cadena que coincida con un patrón. Veamos un ejemplo:

```Rust
let texto = "¡Reemplaza esto, por favor!";

let nuevo_texto = texto.replace("esto,", "nada");

println!("{}", nuevo_texto);

// Output: ¡Reemplaza nada por favor!
```

En este caso, utilizamos `replace` para reemplazar la palabra "esto," por "nada". Sin embargo, también podemos usar esta función para eliminar caracteres, simplemente pasando una cadena vacía como segundo argumento:

```Rust
let texto = "Elimina los espacios en blanco de      esta cadena";

let nuevo_texto = texto.replace(" ", "");

println!("{}", nuevo_texto);

// Output: Eliminalosespaciosenblancodeestacadena
```

## Profundizando

Rust también ofrece la función `trim_matches` para eliminar caracteres del inicio y final de una cadena. Esto es útil si necesitamos limpiar una cadena antes de realizar alguna operación con ella. Por ejemplo:

```Rust
let texto = "¡¡¡Elimina las exclamaciones al inicio y al final!!!";

let nuevo_texto = texto.trim_matches('!');

println!("{}", nuevo_texto);

// Output: Elimina las exclamaciones al inicio y al final
```

También podemos utilizar expresiones regulares para encontrar y eliminar caracteres que coincidan con ciertos patrones. Estas expresiones regulares pueden ser más complejas y nos permiten una mayor flexibilidad en la eliminación de caracteres no deseados.

## Ver también

- [La librería estándar de Rust](https://doc.rust-lang.org/std/)
- [Expresiones regulares en Rust](https://docs.rs/regex/1.3.9/regex/)