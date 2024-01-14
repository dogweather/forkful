---
title:    "Rust: Uniendo cadenas de texto"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo combinar varias cadenas de texto en un solo dato en un programa de Rust? Si la respuesta es sí, entonces estás en el lugar correcto. En esta publicación, vamos a explorar el proceso de concatenación de cadenas de texto en Rust y por qué puede ser útil para tus proyectos de programación.

## Cómo hacerlo

Para concatenar cadenas de texto en Rust, debes utilizar el operador `+`. Este operador permite unir dos o más cadenas de texto para crear una nueva cadena de texto combinada. Veamos un ejemplo práctico:

```Rust
let cadena_uno = "¡Hola, ";
let cadena_dos = "amigos!";
let cadena_completa = cadena_uno + cadena_dos;

println!("{}", cadena_completa);
```

Este código imprimirá la siguiente salida:

```
¡Hola, amigos!
```

Aquí, utilizamos el operador `+` para unir la cadena `"¡Hola, "` con la cadena `"amigos!"`. Luego, almacenamos el resultado en una nueva variable llamada `cadena_completa`. Finalmente, imprimimos el valor de `cadena_completa` utilizando `println!()`.

También puedes utilizar el método `format!()` para concatenar cadenas de texto en Rust. Este método toma una plantilla y un número indefinido de argumentos de cadena y crea una nueva cadena de texto combinada. Por ejemplo:

```Rust
let nombre = "María";
let edad = 25;
let saludo = format!("¡Hola, soy {} y tengo {} años!", nombre, edad);

println!("{}", saludo);
```

La salida de este código sería:

```
¡Hola, soy María y tengo 25 años!
```

## Profundizando

Ahora que ya sabes cómo concatenar cadenas de texto en Rust, es importante que tengas en cuenta algunos detalles importantes. Por ejemplo, el operador `+` solo funciona con dos cadenas de texto al mismo tiempo. Si necesitas unir más de dos cadenas, deberás encadenar varias operaciones de concatenación o utilizar el método `format!()`.

Además, debes tener en cuenta que al concatenar cadenas de texto, estás creando una nueva cadena en memoria en lugar de modificar las cadenas existentes. Esto puede tener un impacto en el rendimiento de tu programa, especialmente si estás trabajando con grandes cantidades de datos. Para optimizar el rendimiento, puedes utilizar el tipo de datos `String` en lugar de `&str` al trabajar con cadenas de texto.

## Vea también

- [Documentación oficial de Rust sobre concatenación de cadenas de texto](https://doc.rust-lang.org/std/string/struct.String.html#concatenating-strings)
- [Ejemplos de código de concatenación de cadenas de texto en Rust](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=61f7dca13edf64fbf554af966ace13d9)