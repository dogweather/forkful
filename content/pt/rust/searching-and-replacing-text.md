---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Artigo de Programação Rust: Pesquisando e Substituindo o Texto

## O Que é e Porquê?

A pesquisa e substituição de texto são operações usadas para localizar sequências de caractere num dado texto e eventualmente substituir por outras sequências. É algo crucial para os programadores por diversas razões, tais como refatoração de código e manipulação de dados.


## Como fazer:

Aqui está um exemplo de como fazer pesquisa e substituição de texto com Rust:

```Rust
let mut mensagem = String::from("Olá, Universo");
mensagem = mensagem.replace("Universo", "Mundo");
println!("{}", mensagem);
```

Saída:

```
Olá, Mundo
```

Neste exemplo, o `.replace` faz o trabalho de pesquisar por "Universo", substituindo-o por "Mundo" e então imprime a nova string.

## Um Mergulho Profundo:

A pesquisa e substituição de texto não são algo novo. Desde os primeiros editores de texto, tal operação existe. Na linguagem Rust, pode-se usar métodos como `.replace` e `.replacen`.

A `.replace` é uma função de alto nível bastante direta. No entanto, você pode querer mais controle sobre a operação de pesquisa e substituição. Para isso, você pode usar as expressões regulares do Rust:

```Rust
use regex::Regex;

let re = Regex::new("Universo").unwrap();
let resultado = re.replace_all("Olá, Universo", "Mundo");
println!("{}", resultado);
```

Isto permite maior flexibilidade ao corresponder strings que sejam mais complexas do que a correspondência direta simples.

## Veja Também:

Aqui estão alguns links úteis se você desejar se aprofundar no assunto:

1. [Documentação oficial da Rust](https://doc.rust-lang.org/book/) - Um excelente ponto de partida.
2. [Biblioteca Regex em Rust](https://docs.rs/regex/1.5.4/regex/) - Documentação oficial para expressões regulares em Rust.
3. [Livro de Receitas de Rust](https://rust-lang-nursery.github.io/rust-cookbook/) - Várias soluções para problemas comuns em Rust, incluindo pesquisas de texto e substituição.