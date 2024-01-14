---
title:                "Rust: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Rust?

Escrever testes em Rust é uma prática importante que pode trazer muitos benefícios para o desenvolvedor e para o projeto em si. Testes garantem a qualidade e funcionalidade do código, reduzem a ocorrência de bugs e permitem uma manutenção mais fácil e rápida. Além disso, escrever testes pode acelerar o processo de desenvolvimento, já que o código é testado automaticamente.

## Como escrever testes em Rust

Para escrever testes em Rust, é preciso utilizar a biblioteca de testes padrão do Rust, a "std::test". Esta biblioteca fornece uma série de macros que permitem definir funções de teste, benchmarks e asserções.

### Exemplos de código:

```Rust
// Importando a biblioteca de testes
use std::test;

// Definindo uma função de teste
#[test]
fn test_addition() {
    let result = add(2, 3);
    assert_eq!(result, 5);
}

// Definindo uma função de benchmark
#[bench]
fn bench_addition(b: &mut Bencher) {
    b.iter(|| add(2, 3));
}
```

A saída do teste seria:

```
running 2 tests
test test_addition ... ok
test bench_addition ... bench:          10 ns/iter (+/- 2)

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

## Mergulho profundo

Para escrever testes efetivos em Rust, é importante entender os conceitos de assertivas (assertions) e testes automatizados. Assertivas são declarações que devem ser verdadeiras para o teste passar. Já os testes automatizados são codificados para serem executados automaticamente, o que torna a tarefa de testar o código mais fácil e precisa.

Além disso, é importante saber como estruturar os testes de forma organizada e como utilizar as diferentes macros da biblioteca std::test, como a `assert!()`, `assert_eq!()` e `assert_ne!()`.

## Veja também

- [Documentação oficial de testes em Rust](https://doc.rust-lang.org/std/test/)
- [Artigo sobre testes em Rust no blog da Rust Brasil](https://www.rust-lang-br.org/2019/04/24/testes-em-rust.html)
- [Vídeo sobre testes em Rust no canal do Rust Brazil](https://youtu.be/KlLwB-w2YyQ)