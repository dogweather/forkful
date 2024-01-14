---
title:    "Rust: Encontrando la longitud de una cadena"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por qué

En Rust, encontrar la longitud de una cadena de texto es una habilidad fundamental que puede ayudarte a comprender mejor cómo funcionan los datos en un programa y cómo manipularlos de manera efectiva. Además, esta es una tarea frecuente en la programación, por lo que es importante comprender cómo hacerlo en Rust.

## Cómo hacerlo

Para encontrar la longitud de una cadena de texto en Rust, puedes utilizar el método `.len()`. Este método devuelve la cantidad de bytes en una cadena, lo que es equivalente a su longitud. Aquí hay un ejemplo de cómo usarlo:

```Rust
let cadena = "¡Hola mundo!";
let longitud = cadena.len();
println!("La longitud de la cadena es: {}", longitud);
```
**Salida:** La longitud de la cadena es: 12

## Profundizando

Es importante mencionar que `.len()` cuenta el número de bytes en una cadena y no el número de caracteres. Esto puede ser confuso si estás trabajando con cadenas multibyte. Para contar el número de caracteres en una cadena, puedes utilizar el método `.chars().count()` de la siguiente manera:

```Rust
let cadena = "¡Hola mundo!";
let caracteres = cadena.chars().count();
println!("La cantidad de caracteres en la cadena es: {}", caracteres);
```
**Salida:** La cantidad de caracteres en la cadena es: 11

Otra forma de encontrar la longitud de una cadena de texto es utilizando un bucle `for`. Esto es especialmente útil si necesitas hacer algún otro procesamiento en cada carácter de la cadena. Aquí hay un ejemplo de cómo se vería:

```Rust
let cadena = "¡Hola mundo!";
let mut contador = 0;
for _ in cadena.chars() {
    contador += 1
}
println!("La longitud de la cadena es: {}", contador);
```
**Salida:** La longitud de la cadena es: 11

## Ver también

- [Documentación oficial de Rust sobre el tipo de datos `str`](https://doc.rust-lang.org/std/primitive.str.html)
- [Tutorial de Rust en español](https://diegorbaquero.github.io)[
- [Rust en 5 minutos](https://www.rust-lang.org/es-ES/)