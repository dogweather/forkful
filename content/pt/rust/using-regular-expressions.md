---
title:                "Rust: Utilizando expressões regulares"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Rust?

As expressões regulares são uma ferramenta poderosa e versátil para manipular texto em qualquer linguagem de programação, incluindo Rust. Com elas, é possível realizar tarefas como buscar e substituir padrões de texto, extrair informações de strings e validar dados de entrada. Além disso, o suporte nativo a expressões regulares em Rust torna seu uso eficiente e seguro.

## Como usar expressões regulares em Rust

Para utilizar expressões regulares em Rust, é necessário importar a biblioteca `regex` no seu código. Em seguida, é possível criar um objeto `Regex` com o padrão de regex desejado e aplicá-lo em uma string com o método `find()` ou `replace()`. Veja um exemplo abaixo:

```Rust
use regex::Regex;

let re = Regex::new(r"(\d{2})/(\d{2})/(\d{4})").unwrap();
let text = "Hoje é dia 30/06/2021".to_string();

let new_text = re.replace(&text, "$2-$1-$3");
// novo texto: Hoje é dia 06-30-2021
```

Além disso, há diversos métodos e funcionalidades disponíveis na biblioteca `regex`, como a possibilidade de capturar grupos em uma expressão, buscar todas as ocorrências de um padrão em uma string e muito mais. É importante se familiarizar com esses recursos para aproveitar ao máximo o potencial das expressões regulares em Rust.

## Passe mais tempo se aprofundando em expressões regulares

As expressões regulares podem parecer complicadas no início, mas com um pouco de prática e conhecimento, elas se tornam uma ferramenta valiosa em seu arsenal de programação. É interessante aprender mais sobre as convenções e sintaxe utilizadas em regex, além de explorar exemplos práticos para entender melhor como aplicá-las em diferentes situações. Alguns recursos úteis para se aprofundar nessa ferramenta são:

- O livro ["The Rust Regex Book"](https://danielkeep.github.io/practical-intro-to-regex/) contém explicações detalhadas e exemplos de uso de expressões regulares em Rust.
- O site [Regex101](https://regex101.com/) permite testar e depurar suas expressões regulares em uma interface interativa.
- A documentação oficial da biblioteca `regex` oferece uma visão geral das funcionalidades e métodos disponíveis.

## Veja também

- [Documentação da biblioteca regex em Rust](https://docs.rs/regex/latest/regex/)
- [Exemplos de uso de expressões regulares em Rust](https://www.tutorialspoint.com/using-regular-expressions-in-a-rust-program)
- [The Rust Book](https://doc.rust-lang.org/book/) para aprender mais sobre a linguagem de programação em si.