---
title:                "Extrayendo subcadenas"
html_title:           "Rust: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas?

La extracción de subcadenas es una herramienta útil en la programación de Rust para manejar cadenas de texto. Permite acceder a una parte específica de una cadena, lo que puede ser útil en varias situaciones, como la manipulación de datos o la validación de entradas de usuario.

## Cómo hacerlo

Para extraer una subcadena en Rust, utilizamos el método `get()` en un objeto `String` o `&str`. Luego, especificamos el rango de la subcadena que deseamos obtener utilizando la sintaxis de rebanado (`..`). Por ejemplo:

```Rust
// Creamos una cadena de texto
let cadena = "¡Hola, mundo!";

// Extraemos la subcadena que va desde el segundo caracter hasta el final de la cadena.
let subcadena = cadena.get(1..);

// Imprimimos la subcadena
println!("{}", subcadena); // Output: "Hola, mundo!"
```

En el ejemplo anterior, usamos `get(1..)` para obtener una subcadena que comienza en el segundo caracter y se extiende hasta el final de la cadena. El primer caracter tiene un índice de 0, por lo que el segundo caracter tiene un índice de 1.

También podemos utilizar la sintaxis de rebanado para especificar un rango de índices. Por ejemplo, si queremos extraer los caracteres en la posición 3 y 4 de la cadena, hacemos lo siguiente:

```Rust
// Creamos una cadena de texto
let cadena = "¡Hola, mundo!";

// Extraemos la subcadena que va desde el tercer hasta el quinto caracter.
let subcadena = cadena.get(2..4);

// Imprimimos la subcadena
println!("{}", subcadena); // Output: "l, "
```

## Profundizando en la extracción de subcadenas

El método `get()` nos permite extraer subcadenas de forma segura, ya que controla los límites de los índices para evitar errores como desbordamiento o subcadena vacía. Además, también podemos utilizar otros métodos como `slice()` y `get_mut()` para obtener y modificar subcadenas de manera eficiente.

Es importante tener en cuenta que en Rust, las cadenas son UTF-8, lo que significa que cada caracter ocupa un número variable de bytes. Esto puede afectar la forma en que se manejan los índices al extraer subcadenas, por lo que es importante tener en cuenta esta diferencia al trabajar con texto.

## Vea también

- Official Rust documentation on Strings: https://doc.rust-lang.org/std/string/index.html
- Rust By Example: Strings: https://doc.rust-lang.org/stable/rust-by-example/std/str.html
- "Rust for Beginners" series on extracting substrings: https://www.youtube.com/watch?v=v-2yFMZXomw&list=PL559kmNvPxSjAIlwBlxxLM9nOF32gThiw