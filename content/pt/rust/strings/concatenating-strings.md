---
date: 2024-01-20 17:36:02.655305-07:00
description: "Concatenar strings \xE9 o ato de juntar duas ou mais sequ\xEAncias de\
  \ caracteres para formar uma nova string. Programadores fazem isso para construir\
  \ mensagens,\u2026"
lastmod: 2024-02-19 22:05:05.393303
model: gpt-4-1106-preview
summary: "Concatenar strings \xE9 o ato de juntar duas ou mais sequ\xEAncias de caracteres\
  \ para formar uma nova string. Programadores fazem isso para construir mensagens,\u2026"
title: Concatenando strings
---

{{< edit_this_page >}}

## O Que & Porquê?

Concatenar strings é o ato de juntar duas ou mais sequências de caracteres para formar uma nova string. Programadores fazem isso para construir mensagens, gerar saídas de texto dinâmicas ou simplesmente combinar dados para processamento.

## Como Fazer:

```Rust
fn main() {
    // Usando o operador `+`
    let saudacao = "Olá".to_string();
    let mundo = " mundo!";
    let frase = saudacao + mundo; 
    println!("{}", frase); // Output: Olá mundo!

    // Usando o método `format!`
    let nome = "Rust";
    let versao = "1.63";
    let frase_formatada = format!("Bem-vindo ao {} versão {}!", nome, versao);
    println!("{}", frase_formatada); // Output: Bem-vindo ao Rust versão 1.63!

    // Concatenando várias strings com o método `push_str`
    let mut mensagem = String::new();
    mensagem.push_str("Juntando");
    mensagem.push_str(" pedaços");
    mensagem.push_str(" de texto.");
    println!("{}", mensagem); // Output: Juntando pedaços de texto.
}
```

## Aprofundamento

Concatenar strings é essencial e não é nada novo. Em linguagens clássicas como C, era comum usar funções como `strcat` para isso, mas Rust moderniza o processo com segurança de tipo e de memória. Outras maneiras de concatenar strings incluem usar o método `join` da trait `Iterator` se você tiver uma coleção de strings. Quando se trata de desempenho, `format!` pode ser menos eficiente que outras técnicas, pois cria uma nova String a cada chamada, enquanto métodos como `push_str` e `+` modificam a String existente. Internamente, Rust gerencia o layout da string na memória usando um `Vec<u8>` para armazenar os dados.

## Veja Também

- A [documentação oficial do Rust](https://doc.rust-lang.org/stable/std/string/index.html) sobre o tipo `String`.
- O `book` do Rust, com um [capítulo sobre strings](https://doc.rust-lang.org/book/ch08-02-strings.html).
- Uma exploração detalhada do [módulo std::str](https://doc.rust-lang.org/std/str/), que lida com strings de fatia, ou ‘slice’ strings, que são vistas frequentes no código Rust.
