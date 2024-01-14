---
title:                "Rust: Convirtiendo una cadena a minúsculas"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué
En la programación, a menudo nos encontramos con la necesidad de manipular y modificar cadenas de texto. Una de estas tareas comunes es convertir una cadena de texto a minúsculas. Ya sea para estandarizar el formato de entrada de los usuarios o para realizar búsquedas en texto sin importar mayúsculas o minúsculas, la conversión a minúsculas es una habilidad útil en cualquier lenguaje de programación. En esta entrada de blog, exploraremos cómo podemos lograr esto en Rust.

## Cómo hacerlo
Para convertir una cadena a minúsculas en Rust, podemos utilizar el método `to_lowercase()` de la estructura `String`. Este método devuelve una nueva cadena que es una copia de la original, pero con todas las letras en minúsculas. Veamos un ejemplo:

```Rust
let cadena = "HOLA MUNDO";
let nueva_cadena = cadena.to_lowercase();
println!("{}", nueva_cadena);
// Output: hola mundo
```

En este ejemplo, primero creamos una cadena de texto llamada`cadena` y luego utilizamos el método `to_lowercase()` para convertirla a minúsculas. Finalmente, imprimimos la nueva cadena en la consola y obtenemos nuestra cadena en minúsculas como resultado.

Podemos aplicar este método tanto a cadenas estáticas como a cadenas dinámicas:

```Rust
let cadena = "Hola ";
let otra_cadena = String::from("Mundo");
let nueva_cadena = cadena.to_lowercase();
let otra_nueva_cadena = otra_cadena.to_lowercase();
println!("{}{}", nueva_cadena, otra_nueva_cadena);
// Output: hola mundo
```

## Profundizando
Es importante tener en cuenta que este método no solo convierte letras mayúsculas a minúsculas, sino que también maneja caracteres especiales y caracteres de otros idiomas. Por ejemplo, si tenemos una cadena con acentos o diacríticos, también se convertirán a minúsculas.

Además, el método `to_lowercase()` es inmutable, lo que significa que no modifica la cadena original sino que devuelve una nueva. Esto es útil ya que no queremos alterar la cadena original en caso de que la necesitemos en su formato original más tarde.

Otra cosa a tener en cuenta es que este método solo convierte a minúsculas, pero no capitaliza las letras iniciales en caso de que la cadena comience con una letra mayúscula.

## Ver también
- Documentación oficial de Rust sobre el método `to_lowercase()`: https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase
- Ejemplos de uso del método `to_lowercase()` en diferentes situaciones: https://blog.logrocket.com/converting-strings-uppercase-lowercase-rust/