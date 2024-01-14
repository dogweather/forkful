---
title:    "Rust: Mayúsculas de una cadena"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Si eres nuevo en la programación, es posible que hayas escuchado hablar de Rust. Si ya eres un desarrollador experimentado, probablemente ya estés familiarizado con este lenguaje de programación moderno y poderoso. En este artículo, vamos a explorar cómo capitalizar una cadena de texto en Rust.

## Cómo hacerlo

Para comenzar, necesitaremos importar el módulo 'str' de Rust. Luego, podemos utilizar el método 'to_uppercase()' para convertir una cadena de texto a mayúsculas. Veamos un ejemplo:

```
Rust
use std::str;

fn main(){
    let string = "¡hola mundo!";
    let capitalizado = string.to_uppercase();
    println!("{}", capitalizado);
}
```

Esto producirá el siguiente resultado:

```
¡HOLA MUNDO!
```

También podemos utilizar el método 'chars' para iterar a través de cada carácter de la cadena y utilizar el método 'to_uppercase()' en cada uno de ellos. De esta manera, mantendremos las letras mayúsculas y minúsculas en su lugar original. Veamos otro ejemplo:

```
Rust
use std::str;

fn main(){
    let string = "Hola Mundo";
    let mut capitalizado = String::new();
    for letra in string.chars() {
        capitalizado.push(letra.to_uppercase());
    }
    println!("{}", capitalizado);
}
```

Esto producirá el siguiente resultado:

```
Hola Mundo
```

Incluso podemos capitalizar una cadena de texto sin utilizar el módulo 'str' de Rust. Podemos convertir la cadena a un vector de bytes, capitalizar cada byte y luego convertirlo de nuevo a una cadena de texto. Veamos otro ejemplo:

```
Rust
fn main(){
    let string = "hola mundo";
    let mut capitalizado = String::new();
    for byte in string.bytes() {
        capitalizado.push(byte.to_ascii_uppercase() as char);
    }
    println!("{}", capitalizado);
}
```

Esto producirá el siguiente resultado:

```
HOLA MUNDO
```

## Profundizando

Ahora que ya sabemos cómo capitalizar una cadena de texto en Rust, es importante conocer cómo funciona internamente. En realidad, existen diferentes formas de capitalizar una cadena de texto, pero en todas ellas se aplican dos conceptos fundamentales: convertir los caracteres a su representación numérica y utilizar operaciones matemáticas para transformarlos en su forma mayúscula correspondiente.

En primer lugar, al utilizar el método 'to_uppercase()', Rust utiliza el método 'to_ascii_uppercase()' internamente. Esto convierte cada carácter en un código numérico ASCII y luego realiza una operación matemática para obtener su forma mayúscula. Por otro lado, en el ejemplo donde utilizamos el módulo 'str' de Rust, se usa el método 'chars' para obtener cada carácter y luego se utiliza el método 'to_uppercase()' para convertirlo a mayúscula. Este proceso también se basa en la representación numérica de los caracteres y operaciones matemáticas.

## Ver también
- https://doc.rust-lang.org/std/primitive.str.html
- https://doc.rust-lang.org/std/string/struct.String.html
- https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase