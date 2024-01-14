---
title:                "Rust: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que utilizar a geração de números aleatórios em Rust?

A geração de números aleatórios é uma funcionalidade muito útil em diversos tipos de aplicação, como jogos, simulações e testes de desempenho. Em Rust, essa funcionalidade é suportada pela biblioteca padrão e é muito fácil de implementar.

## Como utilizar a geração de números aleatórios em Rust

Para gerar números aleatórios em Rust, primeiro precisamos importar a biblioteca `rand` usando a diretiva `use`. Em seguida, podemos utilizar a função `thread_rng()` para obter um gerador de números aleatórios baseado na thread atual. Por fim, podemos utilizar esse gerador para gerar números aleatórios em um intervalo específico usando a função `gen_range()`.

```Rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();

    // Gera um número aleatório entre 1 e 10
    let random_number = rng.gen_range(1..=10);

    println!("O número aleatório gerado é {}", random_number);
}
```

Output:

```
O número aleatório gerado é 7
```

## Aprofundando na geração de números aleatórios em Rust

A biblioteca `rand` em Rust utiliza o algoritmo de geração de números aleatórios conhecido como "Mersenne Twister", que é considerado bastante robusto e eficiente. Além disso, é possível customizar o gerador com sementes específicas para obter resultados previsíveis ou criar geradores paralelos para otimizar o desempenho em aplicações multi-thread.

### Sementes previsíveis

Podemos especificar uma semente para o gerador de números aleatórios usando o método `seed()` antes de gerar qualquer número aleatório.

```Rust
use rand::{Rng, SeedableRng};

fn main() {
    let seed: [u8; 32] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
                          17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32];
    let mut rng = rand::rngs::StdRng::from_seed(seed);

    // Gera um número aleatório entre 1 e 10
    let random_number = rng.gen_range(1..=10);

    println!("O número aleatório gerado com a semente 123456789 é {}", random_number);
}
```

Output:

```
O número aleatório gerado com a semente 123456789 é 4
```

### Geradores paralelos

Em aplicações multi-thread, podemos utilizar o pacote `rand_pcg` para criar geradores de números aleatórios que podem ser compartilhados entre diferentes threads sem causar conflito. Isso pode ser útil em aplicações que demandam um alto desempenho ou precisam gerar números aleatórios de forma independente em diferentes threads.

## Veja também

- [Documentação oficial do pacote `rand` em Rust](https://doc.rust-lang.org/rand)
- [Exemplos de utilização do pacote `rand` em Rust](https://github.com/rust-lang-nursery/rand/tree/master/rand/examples)