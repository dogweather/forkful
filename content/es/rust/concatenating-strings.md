---
title:                "Uniendo cadenas de texto"
html_title:           "Rust: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué concatenar cadenas de texto en Rust?

Concatenar cadenas de texto en Rust es una tarea común en la programación, ya sea para mostrar información al usuario o para construir URLs dinámicas. Al aprender cómo concatenar cadenas en Rust, podrás ampliar tus habilidades de programación y crear aplicaciones más robustas.

## Cómo hacerlo

Para concatenar cadenas de texto en Rust, necesitarás utilizar el operador `+` para unir dos cadenas y crear una nueva. Este es un ejemplo de cómo se vería en código:

```Rust
let hola = "Hola";
let mundo = "mundo";
let saludo = hola + mundo;

println!("{}", saludo); // imprimirá "Hola mundo"
```

Como se puede ver en el ejemplo, primero se definen dos variables con cadenas de texto y luego se crea una tercera variable `saludo` que utiliza el operador `+` para unir las primeras dos variables. Luego, se imprime el valor de `saludo` utilizando el marcador de posición `{}` para mostrar la cadena completa.

También es posible concatenar más de dos cadenas de texto a la vez utilizando el mismo operador `+`. Este es un ejemplo de cómo se vería en código:

```Rust
let personaje = "Mario";
let juego = "Mario Kart";
let copa = "Super";

let descripcion = personaje + " es el protagonista de " + juego + " y ha ganado la copa " + copa;

println!("{}", descripcion); // imprimirá "Mario es el protagonista de Mario Kart y ha ganado la copa Super"
```

También es importante tener en cuenta que el operador `+` solo puede utilizarse en cadenas de texto del mismo tipo, por lo que si se intenta unir una cadena de texto con un tipo numérico, por ejemplo, se obtendrá un error.

## Una mirada más profunda

Cuando se utiliza el operador `+` para unir cadenas de texto, en realidad se está llamando al método `add()` de la estructura `String`. Este método toma la propiedad de la primera cadena y la combina con una referencia a la segunda cadena. Luego, devuelve una nueva cadena que contiene ambas. Este proceso se conoce como "mover" la propiedad de una cadena de texto a otra. En resumen, la cadena de texto original se destruye y se crea una nueva con el resultado de la concatenación.

También es posible utilizar el método `format!()` para concatenar cadenas de texto en Rust. Este método toma como argumentos varias cadenas de texto y las une en una sola, sin afectar las cadenas originales. Este es un ejemplo de cómo se vería en código:

```Rust
let nombre = "Rust";
let fundador = "Mozilla";
let lenguaje = "C++";

let descripcion = format!("{} fue creado por {} utilizando el lenguaje {}", nombre, fundador, lenguaje);

println!("{}", descripcion); // imprimirá "Rust fue creado por Mozilla utilizando el lenguaje C++"
```

En este caso, las cadenas originales se mantienen intactas y se crea una nueva cadena con las mismas.

## Ver también

- [El operador de concatenación en Rust](https://doc.rust-lang.org/std/primitive.str.html#method.add)
- [El método format en Rust](https://doc.rust-lang.org/std/macro.format.html)
- [La estructura String en Rust](https://doc.rust-lang.org/std/string/struct.String.html)