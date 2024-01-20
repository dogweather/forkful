---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Determinar o comprimento de uma string é o processo pelo qual descobrimos o número de caracteres presentes numa string. Os programadores fazem isso principalmente para ajudar na manipulação e validação de dados.

## Como Fazer:

Para descobrir o comprimento de uma string em Rust, podemos usar a função len(). Veja o exemplo abaixo:

```Rust
fn main() {
    let string = "Olá, Mundo!";
    println!("O comprimento da string é: {}", string.len());
}
```

Ao rodar este código, o resultado será:

```Rust
O comprimento da string é: 12
```

## Mergulhando Fundo

1) Contexto Histórico
Rust, desenvolvido pela Mozilla Research, é uma linguagem conhecida por sua segurança e performance, por isso a função len() foi concebida de forma direta para contornar a complexidade decorrente de codificações com diferentes números de bytes por caractere.

2) Alternativas
Se você quiser contar os caracteres Unicode em vez dos bytes, use a função chars(). Esta função considera os caracteres com acentos como um só, diferente da len(). Veja o exemplo:

```Rust
fn main() {
    let s = "olá";
    println!("{}", s.len()); // imprime: 4
    println!("{}", s.chars().count()); // imprime: 3
}
```

3) Detalhes de Implementação
A função len() retorna o número de bytes que a string ocupa, não o número de caracteres Unicode. Em Rust, as strings são codificadas em UTF-8, onde cada caractere Unicode pode ocupar de 1 a 4 bytes.

## Para Saber Mais

- Rust Documentation: https://doc.rust-lang.org/std/string/struct.String.html
- The Rust Programming Language Book: https://doc.rust-lang.org/book/
- Strings in Rust: https://fasterthanli.me/articles/a-half-hour-to-learn-rust