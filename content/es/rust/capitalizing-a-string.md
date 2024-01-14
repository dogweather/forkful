---
title:                "Rust: Cambiando a mayúsculas una cadena"
simple_title:         "Cambiando a mayúsculas una cadena"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué usar Rust para capitalizar cadenas de texto?

Rust es un lenguaje de programación moderno y de código abierto que se ha vuelto popular por ser rápido, seguro y confiable. Una de sus características más útiles es su sistema de tipos de datos que previene errores de memoria y asegura un código robusto. En este artículo, exploraremos cómo se puede utilizar Rust para capitalizar una cadena de texto y por qué es una opción inteligente para cualquier proyecto de programación.

## Cómo hacerlo

Hay varias formas de capitalizar una cadena de texto en Rust, cada una con su propia sintaxis y beneficios. Aquí presentamos dos opciones con código de ejemplo y su salida correspondiente en la consola.

### Uso de la función `to_uppercase`

La forma más sencilla de capitalizar una cadena de texto en Rust es usando la función `to_uppercase`. Esta función tomará una cadena de texto y devolverá una nueva cadena de texto con todas las letras en mayúscula.

```Rust
let texto = "hola mundo";
let texto_capitalizado = texto.to_uppercase();

println!("{}", texto_capitalizado);
```
Salida:
```
HOLA MUNDO
```

### Uso de la macro `println!` y el operador `..`

También podemos capitalizar una cadena de texto utilizando la macro `println!` y el operador `..`. Este método toma la cadena de texto original y la coloca dentro de un nuevo string con mayúsculas.

```Rust
let texto = "hola mundo";

println!("{}!", texto[..].to_uppercase());
```
Salida:
```
HOLA MUNDO!
```

## Profundizando

Además de las dos opciones mencionadas anteriormente, existen otras formas de capitalizar cadenas de texto en Rust. Algunos desarrolladores prefieren utilizar la función `chars` para recorrer y modificar cada uno de los caracteres de la cadena. También se pueden utilizar bibliotecas externas como `rust-string-case` para tener más opciones y personalizar el formato de capitalización.

Una de las principales ventajas de utilizar Rust para capitalizar cadenas de texto es su seguridad y velocidad. Al ser un lenguaje compilado, permite una ejecución más rápida y eficiente en comparación con lenguajes interpretados como JavaScript o Python. Además, su sistema de tipos de datos asegura que el código sea robusto y evita errores a la hora de manipular cadenas de texto.

## Ver también

- [Cómo utilizar las cadenas de texto en Rust](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Documentación de la función `to_uppercase` en Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase)
- [Rust-String-Case: biblioteca para manipulación de cadenas de texto en Rust](https://github.com/whitfin/rust-string-case)