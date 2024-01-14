---
title:    "Rust: Convirtiendo una cadena a minúsculas"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## ¿Por qué?

Convertir una cadena de texto a minúsculas puede ser útil en muchas situaciones. Por ejemplo, facilita la comparación de cadenas de texto sin importar si contienen mayúsculas o minúsculas. También puede ser necesario para cumplir con ciertos requisitos de formato o para mostrar correctamente los datos en pantallas o archivos.

## Cómo hacerlo

Para convertir una cadena de texto a minúsculas en Rust, podemos utilizar el método `.to_lowercase()`. Veamos un ejemplo:

```Rust
let cadena = "HOLA MUNDO";
let min = cadena.to_lowercase();

println!("{}", min);

```
**Salida:**
```
hola mundo
```
Aquí primero creamos una variable `cadena` con un texto en mayúsculas, y luego aplicamos el método `.to_lowercase()` para convertirlo a minúsculas. Finalmente, imprimimos el resultado en la consola.

También podemos utilizar este método para convertir una cadena de texto en minúsculas directamente al momento de su declaración. Por ejemplo:

```Rust
let min = String::from("HEMOS CAMBIADO") .to_lowercase();
```

## Deep Dive

Es importante tener en cuenta que la conversión a minúsculas puede variar dependiendo del idioma en el que estemos trabajando. En Rust, el método `.to_lowercase()` utiliza el estándar Unicode para convertir la cadena, por lo que puede manejar adecuadamente caracteres especiales y acentos de diferentes idiomas.

Además, este método devuelve una nueva cadena de texto en minúsculas, sin modificar la cadena original. Esto es importante en programación funcional, donde se evita cambiar el estado de una variable.

## Ver también

- Documentación oficial de Rust sobre el método `to_lowercase()`: https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase
- Ejemplos prácticos de conversión de cadenas a minúsculas en Rust: https://www.rust-lang.org/learn/get-started#to-lowercase
- Más información sobre manejo de cadenas en Rust: https://blog.logrocket.com/string-manipulation-in-rust/