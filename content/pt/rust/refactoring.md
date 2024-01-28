---
title:                "Refatoração"
date:                  2024-01-26T03:36:46.299164-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refatoração"
programming_language: "Rust"
category:             "Rust"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/refactoring.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Refatoração é o processo de reestruturação do código de computador existente—mudando a fatoração—sem alterar seu comportamento externo. Programadores fazem isso para melhorar atributos não funcionais do software, como legibilidade, redução de complexidade, melhoria da manutenibilidade e criação de uma arquitetura interna ou modelo de objeto mais expressivo para melhorar a extensibilidade.

## Como fazer:

Vamos refatorar um simples trecho de código em Rust para torná-lo mais idiomático e manutenível. Começamos com uma função que calcula a soma de um vetor de inteiros:

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("A soma é {}", sum(&numbers));
}
```

Saída:
```
A soma é 15
```

Agora, vamos refatorar isso para usar Rust mais idiomático, aproveitando iteradores e o método `fold`:

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("A soma é {}", sum(&numbers));
}
```

Sem mudança na saída—ainda é `15`—mas a versão refatorada é mais limpa e usa as forças do Rust como empréstimos e métodos iteradores.

## Aprofundando

A refatoração tem suas raízes na comunidade Smalltalk e foi popularizada no mundo Java pelo livro de Martin Fowler "Refactoring: Improving the Design of Existing Code". Seus princípios são universais e aplicam-se ao Rust também, onde segurança e concorrência são primordiais. Rust encoraja a escrita de código robusto, capturando problemas em tempo de compilação, então, durante a refatoração, o compilador Rust atua como uma rede de segurança.

Alternativas à refatoração manual incluem o uso de ferramentas automatizadas, como 'rustfmt' para formatação de código e 'clippy' para linting, que podem sugerir maneiras mais idiomáticas de escrever código. No entanto, refatorações profundas frequentemente requerem um entendimento cuidadoso do design do código, o que essas ferramentas não podem automatizar completamente.

Em Rust, a refatoração pode girar em torno da melhoria do uso de tipos, aproveitamento de lifetimes efetivamente, redução de alocações desnecessárias, ou emprego de padrões de concorrência como o uso de `Arc<Mutex<T>>` quando necessário. Também é comum a transição de `unwrap()` para um tratamento de erro mais expressivo com `Result<T, E>`.

## Veja Também

Para se aprofundar mais na refatoração em Rust:

- O Livro do Rust: https://doc.rust-lang.org/book/
- Rust por Exemplo: https://doc.rust-lang.org/rust-by-example/
- Clippy, uma ferramenta de linting do Rust: https://github.com/rust-lang/rust-clippy
- "Refactoring: Improving the Design of Existing Code" por Martin Fowler: https://martinfowler.com/books/refactoring.html
