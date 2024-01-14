---
title:    "Rust: Uniendo cadenas"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

# ¿Por qué concatenar cadenas en Rust?

En programación, concatenar cadenas es el proceso de unir dos o más cadenas de texto en una sola cadena. En Rust, esta operación se puede realizar mediante el método `push_str()`, que agrega una cadena al final de otra. Entonces, ¿por qué debería interesarte aprender a concatenar cadenas en Rust? ¡Sigue leyendo para descubrirlo!

## Cómo hacerlo en Rust

Para concatenar cadenas en Rust, debemos utilizar el método `push_str()` dentro de un bucle `for` para iterar a través de una lista de cadenas. Dentro de este bucle, podemos simplemente usar el operador `+` para unir las cadenas deseadas. Veamos un ejemplo de código:

```Rust
let lista_cadenas = ["Hola", "mundo", "desde", "Rust"];
let mut resultado = String::new();

for cadena in lista_cadenas {
    resultado.push_str(cadena);
    resultado.push_str(" ");
}

println!("Resultado: {}", resultado);
```

En este código, primero creamos una lista de cadenas llamada `lista_cadenas` con algunas palabras. Luego, declaramos una variable `resultado` como una cadena vacía y usamos un bucle `for` para iterar a través de la lista. Dentro del bucle, usamos `push_str()` para agregar cada cadena al final de `resultado`, seguida por un espacio en blanco. Finalmente, imprimimos el resultado como una cadena completa.

La salida sería:

```
Resultado: Hola mundo desde Rust
```

También podemos utilizar el operador `+` para concatenar directamente las cadenas, sin necesidad de usar el método `push_str()`. Sin embargo, es importante tener en cuenta que esto crea una nueva cadena en lugar de modificar la existente, lo que puede ser menos eficiente en términos de rendimiento.

## Profundizando en la concatenación de cadenas en Rust

En Rust, la concatenación de cadenas es un proceso bastante simple que se facilita con los métodos `push_str()` y `+`, pero también existen otras opciones para realizar esta operación, como el uso de macros o la función `write!()`. También es importante mencionar que en Rust, las cadenas siempre se codifican en UTF-8, lo que puede afectar el resultado de la concatenación de ciertas cadenas.

Además, en Rust es posible concatenar cadenas de forma más eficiente y segura utilizando el tipo `String`, que permite un control más preciso sobre la memoria y la mutación de las cadenas.

Por último, es importante mencionar que la concatenación de cadenas puede ser útil en una amplia variedad de situaciones en la programación, como la construcción de mensajes de error, la generación de contenido dinámico en una página web o la manipulación de datos en general.

## Ver también

- [Documentación oficial de Rust sobre cadenas](https://doc.rust-lang.org/std/string/struct.String.html)
- [Otros métodos útiles para trabajar con cadenas en Rust](https://www.guru99.com/rust-strings-text.html)
- [Un artículo sobre cómo mejorar la eficiencia en la concatenación de cadenas en Rust](https://docs.rs/snailquote/0.3.0/snailquote/)
- [Ejemplos de concatenación de cadenas en proyectos reales de Rust](https://github.com/cobanermani456/rust-string-concatenation)