---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extração de Substrings em Rust

## O Que? E Por quê? 

Extrair substrings é um método comum de manipular strings em programação, onde removemos uma "sub" string de uma string maior. Programadores fazem isso para filtrar a informação e tornar os dados mais fáceis de manipular ou entender.

## Como fazer:

Extrair uma substring de uma string em Rust é direto.

```Rust
fn main() {
    let string = "Olá, Mundo!";
    let substring = &string[0..4]; // Extrair substring
    println!("{}", substring); // Imprime: "Olá,"
}
```
No exemplo acima, extraímos a substring "Olá," da string "Olá, Mundo!".

## Mergulho Profundo 

Rust usa UTF-8 para encoding de strings. Originalmente, Rust usava um método diferente onde substrings eram baseadas em bytes, mas isso criou problemas com encodings multibyte como UTF-8.

Alternativas para extrair substrings no Rust incluem Method Chaining e usando bibliotecas terceiras como o `substring` crate, que fornece um método `substring` para o tipo de dados String.

A implementação atual de extração de substring em Rust agora faz uso de um índice de caracteres em vez de um índice de bytes como era no passado. Isso significa que agora suporta strings multibyte sem problemas de caráteres multibyte sendo divididos.

## Veja também

- Documentação oficial do Rust em manipulação de strings: https://doc.rust-lang.org/book/ch08-02-strings.html
- `substring` crate: https://crates.io/crates/substring
- Método Chaining em Rust: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html
- Informações sobre o padrão UTF-8: http://www.utf8everywhere.org/