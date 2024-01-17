---
title:                "Interpolación de una cadena"
html_title:           "Rust: Interpolación de una cadena"
simple_title:         "Interpolación de una cadena"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
La interpolación de cadenas es una técnica utilizada por los programadores para insertar variables en un texto de manera dinámica. Esto permite crear cadenas personalizadas en función del valor de las variables, lo que resulta muy útil en situaciones como la generación de mensajes de error o la creación de mensajes de bienvenida personalizados.

## Cómo hacerlo:
Para interpolar una cadena en Rust, simplemente utilizamos las llaves {} dentro de la cadena para indicar dónde deseamos insertar la variable. A continuación, pasamos las variables como argumentos en la función `format!()` y Rust se encarga de insertar los valores correspondientes en la posición indicada. Por ejemplo:

```Rust
let nombre = "Juan";
let edad = 30;
let mensaje = format!("¡Hola, {}! ¡Tienes {} años!", nombre, edad);
println!("{}", mensaje);
// Output: ¡Hola, Juan! ¡Tienes 30 años!
```

## Profundizando:
La interpolación de cadenas no es una técnica exclusiva de Rust, sino que es común en muchos otros lenguajes de programación. Sin embargo, en Rust es especialmente importante debido a la seguridad de tipos y a las limitaciones en la manipulación directa de cadenas. En lugar de modificar directamente la cadena, Rust prefiere crear una nueva cadena con los valores interpolados, lo que evita posibles vulnerabilidades.

## Ver también:
- Documentación oficial de Rust sobre interpolación de cadenas: https://doc.rust-lang.org/stable/book/ch08-02-strings.html#formatting-strings
- Ejemplos de interpolación de cadenas en otros lenguajes de programación: https://www.java-tips.org/other-api-tips-100035/4368-what-is-string-interpolation-.html