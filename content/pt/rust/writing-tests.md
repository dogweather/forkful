---
title:                "Rust: Escrevendo testes"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-tests.md"
---

{{< edit_this_page >}}

# Por que escrever testes em Rust é importante?

Testes são parte essencial do processo de desenvolvimento de software, e isso não é diferente quando se trata de programação em Rust. Escrever testes pode ajudar a garantir que seu código está funcionando corretamente e evitar possíveis bugs antes mesmo deles acontecerem. Além disso, testes também podem servir como documentação do seu código, facilitando a compreensão para outros desenvolvedores.

## Como escrever testes em Rust

Em Rust, os testes são escritos usando o módulo `test` e a macro `#[test]`. Veja um exemplo de como escrever um teste simples para uma função que verifica se um número é par:

```rust
#[test]
fn test_par() {
    assert!(is_par(2));
    assert!(!is_par(3));
}
```
O primeiro passo é importar o módulo `test` com `use std::test`. Em seguida, usamos a macro `#[test]` antes da função de teste. Dentro do teste, usamos a macro `assert!` para verificar se a função `is_par` retorna o resultado esperado. Se a asserção falhar, o teste irá ser marcado como falho.

Além disso, também é possível usar a macro `assert_eq!` para verificar a igualdade de valores e `assert_ne!` para verificar a diferença entre eles.

## Aprofundando nos testes

Em Rust, também é possível escrever testes que verificam se um código produz um erro esperado. Isso é feito usando a macro `should_panic` antes da função de teste. Veja um exemplo:

```rust
#[test]
#[should_panic]
fn test_divisao_por_zero() {
    println!("{}", 10 / 0);
}
```

Além disso, também é possível testar o desempenho do seu código usando a macro `#[bench]`. Isso ajuda a identificar partes do seu código que podem afetar o desempenho geral do seu programa.

Outra alternativa é usar a biblioteca `quickcheck` para gerar dados de teste aleatórios e verificar se o seu código funciona para uma variedade de casos.

## Veja também

- [Documentação oficial do módulo `test` em Rust](https://doc.rust-lang.org/std/test/index.html)
- [Rust by Example: Testes](https://doc.rust-lang.org/rust-by-example/testing.html)
- [The Rust Book: Testes automatizados](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [QuickCheck: Uma biblioteca para propriedade baseada em testes](https://crates.io/crates/quickcheck)

Escrever testes em Rust é uma prática importante para garantir a qualidade do seu código. Além disso, usar as ferramentas e bibliotecas disponíveis pode facilitar o processo de teste e melhorar a qualidade do seu código. Certifique-se de sempre escrever testes para o seu código e refatorá-los conforme necessário para manter a qualidade do seu software.