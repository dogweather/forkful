---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

---

## ¿Qué y por qué?

Convertir una cadena de texto (`string`) a minúsculas implica cambiar todas sus letras mayúsculas en minúsculas. Los programadores lo hacen para normalizar los datos de entrada y facilitar las comparaciones entre cadenas.

## ¿Cómo hacerlo?

Aquí se muestra cómo convertir una cadena en Rust a minúsculas utilizando el método `to_lowercase()`.

```Rust
fn main() {
    let frase = "HOLA, MUNDO!";
    let frase_en_minusculas = frase.to_lowercase();
    println!("{}", frase_en_minusculas);
}
```
Este código producirá:

```Rust
"hola, mundo!"
```

## Un poco más de fondo

1. **Contexto histórico** : El concepto de convertir las letras mayúsculas a minúsculas ha existido desde que se crearon las primeras computadoras, aunque el método exacto ha variado en función del tipo de programación y el idioma en cuestión. 
   
2. **Alternativas** : En la mayoría de los lenguajes de programación existe un método equivalente para convertir una cadena a minúsculas. En Python, por ejemplo, también se utiliza el método `.lower()`, mientras que en JavaScript se utiliza `.toLowerCase()`.
   
3. **Detalles de implementación** : Rust utiliza Unicode para el manejo de caracteres y cadenas de texto, lo que significa que el método `to_lowercase()` de Rust también funciona con caracteres que no son del alfabeto inglés. Sin embargo, ten en cuenta que el método no cambia la cadena original, sino que devuelve una nueva cadena convertida a minúsculas.

## Ver también:

Estos son algunos enlaces útiles para seguir estudiando este tema:

- Documentación oficial de Rust, sección 'str': https://doc.rust-lang.org/std/str/
- Guía de Rust para cadenas de texto: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html
- Documentación de Rust sobre métodos en 'str': https://doc.rust-lang.org/std/str/trait.StrExt.html
---