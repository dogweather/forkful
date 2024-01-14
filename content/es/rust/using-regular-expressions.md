---
title:                "Rust: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué utilizar expresiones regulares en Rust

Las expresiones regulares son una herramienta poderosa en cualquier lenguaje de programación, y Rust no es una excepción. Con ellas, podemos realizar búsquedas y manipulaciones de patrones en cadenas de texto de una manera más eficiente y precisa. Si estás trabajando con Rust y lidiando con cadenas de texto, ¡las expresiones regulares pueden ser tu mejor amiga! 

## Cómo utilizar expresiones regulares en Rust

¡Vamos a sumergirnos en el mundo de las expresiones regulares en Rust! En primer lugar, necesitamos importar la biblioteca `regex`, que nos permitirá trabajar con expresiones regulares. Luego, debemos compilar nuestra expresión regular usando el método `Regex::new()` y pasarle como argumento nuestro patrón entre comillas. Después, podemos utilizar diferentes métodos como `is_match()` para verificar si una cadena cumple con nuestro patrón, o `find()` para encontrar la primera instancia del patrón en la cadena. 

```Rust
use regex::Regex;

// Compilamos nuestra expresión regular
let re = Regex::new(r"\d{3}-\d{2}-\d{4}").unwrap(); 

// Creamos una cadena de texto
let ssn = "123-45-6789";

// Verificamos si la cadena cumple con nuestro patrón
if re.is_match(ssn) {
    println!("¡Número de Seguro Social válido!");
}

// Buscamos la primera instancia del patrón en la cadena
if let Some(mat) = re.find(ssn) {
    println!("El patrón se encuentra en la posición: {:?}", mat);
}
```

La salida de este código sería:

```bash
¡Número de Seguro Social válido!
El patrón se encuentra en la posición: 0..11
```
Podemos ver que la expresión regular ha encontrado una coincidencia en la cadena y también nos ha proporcionado la posición donde se encuentra.

## Profundizando en el uso de expresiones regulares

Hay muchas variantes y opciones que podemos utilizar al trabajar con expresiones regulares en Rust. Por ejemplo, podemos utilizar la sintaxis `(?P<nombre>)` para capturar grupos de nuestro patrón y asignarles un nombre, lo que nos permite acceder a ellos de manera más sencilla. También podemos utilizar la bandera `i` para hacer nuestro patrón insensible a mayúsculas y minúsculas. Hay muchas otras posibilidades y es importante explorar la documentación oficial para aprovechar al máximo el potencial de las expresiones regulares en Rust.

## Vea también

Aquí hay algunos recursos adicionales que pueden ser útiles al trabajar con expresiones regulares en Rust:

- Documentación oficial de Rust sobre expresiones regulares: https://doc.rust-lang.org/std/regex/
- Cheat sheet de expresiones regulares en Rust: https://danielkeep.github.io/regexr/
- Ejemplos prácticos de expresiones regulares en Rust: https://www.codewars.com/kata/59e7202ffc3c4951fd00005b