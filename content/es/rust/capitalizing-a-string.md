---
title:    "Rust: Capitalizando una cadena"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado por qué es importante saber cómo capitalizar una cadena de texto en Rust? Bueno, la respuesta es sencilla: la capitalización es una parte importante del manejo de datos y puede ser útil en muchas aplicaciones, desde formatear nombres en una base de datos hasta mostrar títulos correctamente en una interfaz de usuario. Afortunadamente, Rust ofrece una forma sencilla y eficiente de capitalizar cadenas de texto.

## Cómo hacerlo

Para capitalizar una cadena de texto en Rust, debes utilizar el método `.to_uppercase()` en la cadena deseada. Este método es parte del trait `ToUppercase`, el cual es implementado automáticamente por todos los tipos de datos que pueden ser capitalizados, como `String` y `&str`.

Veamos un ejemplo de cómo capitalizar una cadena de texto en Rust:

```Rust
let my_string = "hola mundo";
let capitalized_string = my_string.to_uppercase();
println!("La cadena original es: {}", my_string);
println!("La cadena capitalizada es: {}", capitalized_string);
```

Si ejecutamos este código, la salida sería:

```
La cadena original es: hola mundo
La cadena capitalizada es: HOLA MUNDO
```

¡Así de fácil! Además, también puedes utilizar el método `to_lowercase()` para convertir una cadena en minúsculas y `to_titlecase()` para convertirla en formato de título.

## Profundizando

Si quieres saber más sobre cómo funciona la capitalización en Rust, debes saber que utiliza el estándar Unicode para determinar qué caracteres deben ser convertidos a mayúsculas y cuáles a minúsculas. Esto significa que no solo funciona con letras del alfabeto latino, sino también con caracteres de otros idiomas y símbolos especiales.

Además, hay una forma de capitalizar solo la primera letra de una palabra en una cadena utilizando el método `.capitalize()` en lugar de `.to_titlecase()`. Esta es una opción útil si solo deseas mostrar la primera letra en mayúscula en un nombre o título, por ejemplo.

Ahora que conoces algunos detalles más sobre la capitalización en Rust, puedes experimentar con diferentes cadenas de texto y ver cómo se comportan los diferentes métodos.

## Ver también

- Documentación de Rust sobre capitalización: https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase
- Página del trait `ToUppercase` en el sitio web de Rust: https://doc.rust-lang.org/std/primitive.str.html#to-ascii_uppercase
- Tutorial de Rust sobre cadenas de texto: https://doc.rust-lang.org/book/ch08-02-strings.html