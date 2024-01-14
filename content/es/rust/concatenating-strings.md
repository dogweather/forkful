---
title:                "Rust: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has encontrado en la situación en la que necesitas unir múltiples cadenas de texto en una sola? Ya sea para mostrar un mensaje personalizado o para construir una URL dinámica, la concatenación de cadenas es una habilidad esencial para cualquier programador. En este blog post, te enseñaremos cómo concatenar cadenas en Rust de manera eficiente y efectiva.

## Cómo hacerlo

¡No te preocupes, concatenar cadenas en Rust es muy sencillo! Primero, debes importar el módulo `String` para poder trabajar con cadenas en tu código:

```Rust
use std::string::String;
```

Ahora, puedes utilizar el operador `+` para unir dos cadenas y crear una nueva. Por ejemplo:

```Rust
let nombre = "María";
let apellido = "González";
let nombre_completo = nombre + apellido;

println!("Hola, mi nombre es {}", nombre_completo); 
// Salida: Hola, mi nombre es María González
```

También puedes utilizar el método `push_str()` para añadir una cadena al final de otra, modificando directamente la cadena original:

```Rust
let mut saludo = String::from("Hola");
saludo.push_str("amigos!");

println!("{}", saludo); // Salida: Hola amigos!
```

Otra forma de concatenar cadenas es utilizando el método `format()`, que te permite combinar varias cadenas y valores de manera más fácil y legible:

```Rust
let nombre = "Juan";
let edad = 25;
let saludo = format!("Hola, mi nombre es {} y tengo {} años", nombre, edad);

println!("{}", saludo); 
// Salida: Hola, mi nombre es Juan y tengo 25 años
```

## Profundizando

Si bien la concatenación de cadenas parece una tarea sencilla, en realidad puede tener un impacto en el rendimiento de tu código. Rust tiene un concepto llamado _ownership_, que se encarga de administrar la memoria de manera eficiente y evitar errores como fugas de memoria o acceso a memoria no válido. Cuando concatenas cadenas en Rust, estás creando una nueva cadena en la memoria y copiando cada una de las cadenas originales. Esto puede ser costoso en términos de rendimiento.

Para mitigar este problema, Rust también ofrece el tipo `String`, que es una cadena dinámica y mutable que puede crecer a medida que se le añaden valores. Al utilizar este tipo en lugar de cadenas estáticas, sólo se copiará la referencia a la cadena en lugar de su contenido, mejorando el rendimiento de tu código.

Otra opción para mejorar la concatenación de cadenas es utilizar la macro `format!()`, que te permite crear una cadena formateada sin tener que crear nuevas cadenas en la memoria.

En resumen, al concatenar cadenas en Rust, siempre debemos tener en cuenta el costo de rendimiento y elegir la opción más adecuada según nuestras necesidades.

## Ver también

- [Documentación oficial sobre cadenas en Rust](https://doc.rust-lang.org/std/string/)
- [Rust By Example: Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [Ejemplos de concatenación de cadenas en Rust](https://www.techiediaries.com/rust-string-concatenation-examples/)

¡Ahora que sabes cómo concatenar cadenas en Rust, no hay límites para lo que puedes crear! Practica y sigue aprendiendo sobre este poderoso lenguaje de programación. ¡Hasta la próxima!