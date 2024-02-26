---
date: 2024-01-20 17:48:19.021037-07:00
description: "Encontrar la longitud de una cadena de texto en Rust nos dice cu\xE1\
  ntos bytes ocupa. Esto importa al trabajar con textos: validar entradas, limitar\
  \ tama\xF1o,\u2026"
lastmod: '2024-02-25T18:49:55.331831-07:00'
model: gpt-4-1106-preview
summary: "Encontrar la longitud de una cadena de texto en Rust nos dice cu\xE1ntos\
  \ bytes ocupa. Esto importa al trabajar con textos: validar entradas, limitar tama\xF1\
  o,\u2026"
title: Calculando la longitud de una cadena
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Encontrar la longitud de una cadena de texto en Rust nos dice cuántos bytes ocupa. Esto importa al trabajar con textos: validar entradas, limitar tamaño, segmentar datos, entre otros.

## Cómo hacerlo:
Ejemplos claros para obtener la longitud de una cadena en Rust:

```Rust
fn main() {
    let mi_cadena = "Hola Mundo";
    println!("La longitud es: {}", mi_cadena.len());
}
```
Salida:
```
La longitud es: 10
```
Si la cadena contiene caracteres Unicode, ten en cuenta que `.len()` devuelve bytes, no caracteres:
```Rust
fn main() {
    let unicode = "¡Hola!";
    println!("La longitud en bytes es: {}", unicode.len());
}
```
Salida:
```
La longitud en bytes es: 7
```

## Profundización
Históricamente, medir una cadena en lenguajes como C era tedioso, contando hasta un carácter nulo. Rust simplifica este proceso pero su enfoque es en bytes, no en caracteres Unicode (escalares). Hay alternativas como `char_count` si necesitas contar caracteres:

```Rust
fn main() {
    let unicode = "¡Hola!";
    let caracteres: Vec<char> = unicode.chars().collect();
    println!("La cantidad de caracteres Unicode es: {}", caracteres.len());
}
```

Salida:
```
La cantidad de caracteres Unicode es: 5
```

La longitud en bytes se utiliza por defecto ya que Rust está diseñado para ser seguro y rápido en operaciones con memoria. Esto implica entender que `String` en Rust es como un `Vec<u8>` bajo el capó.

## Ver También
- Documentación oficial de Rust sobre `String`: [Strings in Rust](https://doc.rust-lang.org/book/ch08-02-strings.html)
- Referencia de la API de Rust para el tipo `String`: [API Reference](https://doc.rust-lang.org/std/string/struct.String.html)
- Rust by Example sobre cadenas: [Rust by Example: Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- Stack Overflow en Español para preguntas específicas: [Stack Overflow en Español](https://es.stackoverflow.com/questions/tagged/rust)
