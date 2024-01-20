---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Conversão de Strings para Minúsculas em Rust: Um Guia Prático

## O Que & Por quê?

Converter string para minúsculas significa transformar todos os caracteres da string de letras maiúsculas para minúsculas. É uma operação comum quando queremos padronizar os dados de entrada para comparações, pesquisas ou visualizações sem diferenciar maiúsculas e minúsculas.

## Como fazer:

Aqui está um exemplo simples:

```rust
fn main() {
    let s = "Olá, MUNDO!";
    println!("{}", s.to_lowercase());
}
```

A saída será: `olá, mundo!`

## Deep Dive

A necessidade de converter strings para minúsculas tem sido parte da programação desde o início. Os primeiros computadores eram sistemas sensíveis ao caso considerando 'A' e 'a' como diferentes. Então a conversão para minúsculas se tornou crucial para comparações de texto sem diferenças.

Existem formas alternativas de fazer isso em Rust. Uma forma é usar um loop e o método `to_ascii_lowercase()`, retornando o caractere convertido para ascii lowercase.

```rust
fn main() {
    let s = "Olá, MUNDO!";
    let lower_s: String = s.chars().map(|c| c.to_ascii_lowercase()).collect();
    println!("{}", lower_s);
}
```

Nota: `to_lowercase()` trabalha com strings unicode, enquanto `to_ascii_lowercase()` somente com strings ASCII. 

## Veja Também

- Documentação Rust oficial sobre `to_lowercase()`: https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase 
- Documentação Rust oficial sobre `to_ascii_lowercase()`: https://doc.rust-lang.org/std/primitive.char.html#method.to_ascii_lowercase 

Este artigo deve lhe oferecer uma base sólida para compreender como e por que as strings são convertidas para minúsculas em Rust.