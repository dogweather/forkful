---
title:                "Rust: Convertendo uma string para minúsculas."
simple_title:         "Convertendo uma string para minúsculas."
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string em letras minúsculas em Rust?

Ao trabalhar com strings em um programa em Rust, às vezes pode ser necessário convertê-las para letras minúsculas. Isso pode ser útil para comparar strings de forma mais precisa ou para garantir que a entrada do usuário esteja em um formato padronizado. Felizmente, a linguagem Rust possui uma função incorporada que permite que você faça isso facilmente.

## Como fazer

Primeiramente, precisamos importar o módulo `ascii` da biblioteca padrão do Rust. Isso permitirá que utilizemos a função `to_ascii_lowercase()` para realizar a conversão desejada. Em seguida, podemos utilizar essa função em uma string da seguinte maneira:

```rust
use std::ascii;

let minha_string = "Olá, Mundo!";
let string_lower = ascii::to_ascii_lowercase(&minha_string);

println!("String original: {}", minha_string);
println!("String em letras minúsculas: {}", string_lower);
```

O resultado do código acima seria:

```
String original: Olá, Mundo!
String em letras minúsculas: olá, mundo!
```

Como podemos ver, a função `to_ascii_lowercase()` converte todas as letras da string para suas respectivas formas minúsculas. É importante notar que essa função trabalha apenas com caracteres ASCII, portanto, caracteres especiais de outros alfabetos ou símbolos não serão afetados.

## Mergulho Profundo

Alguns podem se perguntar por que é necessário utilizar o módulo `ascii` e não apenas a função `to_lowercase()` da própria string em si. A resposta está na diferença entre os métodos.

A função `to_lowercase()` da string irá utilizar as regras de capitalização do sistema operacional em que o programa está sendo executado. Isso pode resultar em diferenças entre a versão de letras minúsculas em diferentes plataformas.

Por outro lado, a função `to_ascii_lowercase()` utilizará as regras de capitalização ASCII, garantindo que a conversão seja consistente em todas as plataformas.

## Veja também

- [Documentação da função to_ascii_lowercase() na biblioteca padrão do Rust](https://doc.rust-lang.org/std/ascii/fn.to_ascii_lowercase.html)
- [Comparando strings em Rust usando letras minúsculas](https://medium.com/@nathaliev/compare-strings-in-rust-by-lowercase-version-28378dfee332)
- [Diferença entre os métodos to_lowercase() e to_ascii_lowercase() em Rust](https://dev.to/nitharshana/to-lowercase-vs-to-ascii-lowercase-in-rust-1g5c)