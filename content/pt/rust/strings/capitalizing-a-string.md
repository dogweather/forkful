---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:25.732336-07:00
description: "Capitalizar uma string em Rust envolve modificar a string de modo que\
  \ seu primeiro caractere seja mai\xFAsculo, caso seja uma letra, enquanto o restante\
  \ da\u2026"
lastmod: '2024-03-13T22:44:46.351495-06:00'
model: gpt-4-0125-preview
summary: "Capitalizar uma string em Rust envolve modificar a string de modo que seu\
  \ primeiro caractere seja mai\xFAsculo, caso seja uma letra, enquanto o restante\
  \ da string permanece inalterado."
title: Capitalizando uma string
weight: 2
---

## O que & Por quê?

Capitalizar uma string em Rust envolve modificar a string de modo que seu primeiro caractere seja maiúsculo, caso seja uma letra, enquanto o restante da string permanece inalterado. Programadores frequentemente realizam essa operação para fins de formatação, tais como preparar palavras para títulos ou garantir consistência na entrada de dados dos usuários.

## Como fazer:

Para capitalizar uma string em Rust, você tem duas principais vias: usar as funcionalidades da biblioteca padrão ou empregar crates de terceiros para necessidades mais complexas ou específicas. Aqui está como você pode fazer ambos.

### Usando a Biblioteca Padrão do Rust

A biblioteca padrão do Rust não oferece um método direto para capitalizar strings, mas você pode alcançar isso manipulando os caracteres da string.

```rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "hello";
    println!("{}", capitalize_first(my_string)); // Saída: Hello
}
```

### Usando o Crate `heck`

Para uma abordagem mais direta, especialmente quando trabalhando dentro de um contexto mais amplo de processamento de texto, você pode preferir usar bibliotecas de terceiros como `heck`. O crate `heck` oferece várias funcionalidades de conversão de caso, incluindo uma maneira simples de capitalizar strings.

Primeiro, adicione `heck` ao seu `Cargo.toml`:

```toml
[dependencies]
heck = "0.4.0"
```

Depois, use-o para capitalizar sua string:

```rust
extern crate heck; // Não necessário na edição Rust 2018 ou posterior
use heck::TitleCase;

fn main() {
    let my_string = "hello world";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // Saída: Hello World
}
```

Nota: O método `to_title_case` fornecido por `heck` capitaliza cada palavra na string, o que pode ser mais do que você está procurando se quiser apenas o primeiro caractere da string capitalizado. Ajuste seu uso de acordo com suas necessidades específicas.
