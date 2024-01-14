---
title:    "Rust: Escrevendo testes"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que escrever testes é importante na programação Rust?

Escrever testes é uma prática importante na programação em qualquer linguagem, mas é especialmente crucial no Rust. Com o Rust, você pode escrever testes que garantem que seu código seja seguro e livre de erros, ajudando a evitar problemas e falhas no futuro.

## Como escrever testes em Rust

Escrever testes em Rust é uma tarefa bastante simples. Você pode usar a macro `assert!` para verificar se uma determinada condição é verdadeira. Por exemplo, se você quiser testar se a função `soma` retorna a soma correta de dois números, você pode escrever um teste dessa forma:

```Rust
#[test]
fn teste_soma() {
    assert!(soma(2, 2) == 4);
}
```

Este teste irá passar se a condição dentro da macro `assert!` for verdadeira, caso contrário, o teste falhará. É uma boa prática ter vários testes para cada função, verificando diferentes cenários e possíveis resultados.

Além disso, o Rust possui uma ferramenta integrada chamada `cargo` que facilita a execução e gerenciamento de testes. Basta executar o comando `cargo test` no diretório do seu projeto para executar todos os testes presentes no código.

## Aprofundando em escrever testes em Rust

Há diversas outras técnicas e funcionalidades no Rust que podem ser usadas para escrever testes de forma mais eficiente e abrangente. Por exemplo, o Rust permite a criação e utilização de testes unitários e testes de integração, que podem ser executados separadamente.

Além disso, é possível utilizar a biblioteca `assert_eq!` para verificar se dois valores são iguais, ao invés de apenas verificar se uma condição é verdadeira. Isso é particularmente útil para testar funções que retornam valores complexos, como estruturas de dados.

## Veja também

- Documentação oficial do Rust sobre testes: https://doc.rust-lang.org/book/ch11-00-testing.html
- Artigo sobre testes em Rust: https://www.oreilly.com/library/view/rust-programming-by/9781788390637/ch07s02.html
- Tutorial de testes em Rust: https://dev.to/darkasura114/unit-testing-in-rust-3a8a