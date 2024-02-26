---
date: 2024-01-20 17:46:25.082824-07:00
description: "Extrair substrings \xE9 o processo de pegar peda\xE7os de uma string\
  \ maior. Fazemos isso para manipular ou analisar partes espec\xEDficas de texto."
lastmod: '2024-02-25T18:49:43.978286-07:00'
model: gpt-4-1106-preview
summary: "Extrair substrings \xE9 o processo de pegar peda\xE7os de uma string maior.\
  \ Fazemos isso para manipular ou analisar partes espec\xEDficas de texto."
title: Extraindo substrings
---

{{< edit_this_page >}}

## What & Why?
Extrair substrings é o processo de pegar pedaços de uma string maior. Fazemos isso para manipular ou analisar partes específicas de texto.

## How to:
```Rust
fn main() {
    let texto = "Olá, programadores Rust!";
    let substring = &texto[7..20]; // Notem que contamos a partir de 0!

    println!("Substring extraída: {}", substring); // Saída: Substring extraída: programadores
}
```

## Deep Dive
Extrair substrings em Rust é um pouquinho diferente de outras linguagens. Ao contrário de Python ou JavaScript, Rust lida com strings de forma mais complexa por causa do sistema de codificação UTF-8, que não usa um número fixo de bytes para cada caractere. Isso significa que você não pode simplesmente dizer "pegue os bytes de 1 a 4" sem correr o risco de cortar um caractere pela metade!

Então, a gente trabalha com slices de strings (`&str`) para obter substrings. Mas temos que ter cuidado para fazer isso nas fronteiras dos caracteres, não dos bytes. Se não, pode dar erro em tempo de execução por conta de uma fatia inválida!

Alternativas incluem usar métodos como `char_indices` para iterar sobre os caracteres com segurança ou usar crates (bibliotecas) como `unicode-segmentation` para lidar com segmentos de texto mais complexos como graphemes.

Antigamente, as strings em Rust eram mais difíceis de manusear, mas a linguagem evoluiu e agora nós temos ótimas ferramentas e documentação para nos ajudar nisso.

## See Also
- [The Rust Programming Language - Book on Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust by Example - Slice](https://doc.rust-lang.org/rust-by-example/primitives/tuples.html)
