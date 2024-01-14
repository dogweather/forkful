---
title:                "Rust: Utilizando expresiones regulares"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué utilizar expresiones regulares en Rust

Las expresiones regulares son una herramienta poderosa para trabajar con texto en cualquier lenguaje de programación, y Rust no es la excepción. Si estás desarrollando una aplicación que necesita manipular cadenas de texto, las expresiones regulares pueden ahorrarte mucho tiempo y esfuerzo al realizar tareas como búsqueda, validación, manipulación y extracción de datos.

## Cómo utilizar expresiones regulares en Rust

Para utilizar expresiones regulares en Rust, primero debes importar el módulo `regex` y crear una nueva expresión regular utilizando la macro `regex!`. Por ejemplo:

````Rust
use regex::Regex;

let re = regex!("hola.*mundo");
````

En este caso, hemos creado una expresión regular que busca una cadena que comience con "hola" y tenga cualquier cantidad de caracteres antes de "mundo". Ahora podemos utilizar esta expresión regular para realizar diferentes tareas, como buscar en una cadena o validar su formato. Por ejemplo:

````Rust
let texto = "hola querido mundo";

if re.is_match(texto) {
    println!("La cadena coincide con la expresión regular");
} else {
    println!("La cadena no coincide con la expresión regular");
}
````

El código anterior imprimirá "La cadena coincide con la expresión regular" ya que nuestra cadena contiene la subcadena "hola querido mundo", que cumple con nuestra expresión regular.

También podemos utilizar las expresiones regulares para extraer partes específicas de una cadena. Por ejemplo, si queremos extraer el nombre de usuario de una dirección de correo electrónico, podemos hacer lo siguiente:

````Rust
let re = Regex::new(r"(.+)@(.+)").unwrap();
let correo = "correo@ejemplo.com";

if let Some(cap) = re.captures(correo) {
    println!("El usuario es: {}", cap.get(1).unwrap().as_str());
} else {
    println!("La cadena no coincide con la expresión regular");
}
````

Este código imprimirá "El usuario es: correo", ya que hemos utilizado grupos de captura en nuestra expresión regular para extraer la parte del usuario antes del símbolo "@".

## Profundizando en el uso de expresiones regulares en Rust

Las expresiones regulares en Rust son muy similares a las de otros lenguajes de programación. Puedes utilizar caracteres especiales como "*" para representar cualquier cantidad de caracteres, o "+" para representar una o más repeticiones del carácter anterior. También puedes utilizar el operador de agrupación "()" para crear grupos de captura dentro de tu expresión regular.

Además, en Rust puedes utilizar expresiones regulares Unicode para trabajar con cadenas en diferentes idiomas y alfabetos.

Es importante tener en cuenta que las expresiones regulares pueden ser costosas en términos de rendimiento, por lo que debes utilizarlas con precaución en aplicaciones de alta velocidad. También es recomendable comprender bien cómo funcionan y probarlas de forma exhaustiva antes de implementarlas en tu código.

## Ver también

- [Documentación oficial de Rust sobre expresiones regulares](https://doc.rust-lang.org/std/regex/)
- [Tutorial de expresiones regulares en Rust por The Rust Programming Language](https://doc.rust-lang.org/book/ch09-06-pattern-syntax.html)
- [Herramienta online de pruebas de expresiones regulares en Rust](https://regex101.com/library/uI5pD3)