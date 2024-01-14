---
title:    "Rust: Gerando números aleatórios"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que usar geração de números aleatórios em Rust?

A geração de números aleatórios é uma técnica importante em muitos tipos de programas, como jogos, simulações e criptografia. Em Rust, existem várias maneiras de gerar números aleatórios, oferecendo flexibilidade e segurança ao mesmo tempo.

## Como fazer isso em Rust?

A maneira mais simples de gerar números aleatórios em Rust é usando a biblioteca padrão `rand`. Primeiro, importamos a biblioteca em nosso código:

```Rust
use rand::Rng;
```

Agora, podemos usar a função `thread_rng()` para gerar um gerador de números aleatórios seguro e inicializá-lo com o método `fill()` para gerar um número aleatório:

```Rust
let mut rng = rand::thread_rng();
let random_number: u32 = rng.fill();
```

Podemos especificar o tipo de número que queremos gerar, como no exemplo acima, onde definimos `u32` para gerar um número inteiro de 32 bits.

Também é possível gerar números aleatórios em um intervalo específico, usando o método `gen_range()`:

```Rust
let random_number: u8 = rng.gen_range(1..=10);
```

Este exemplo irá gerar um número aleatório entre 1 e 10, incluindo esses números.

## Mergulho profundo

Em Rust, a geração de números aleatórios é feita por meio de um gerador de números pseudoaleatórios (PRNG), que é um algoritmo que produz uma sequência de números que parecem ser aleatórios, mas são gerados de maneira determinística.

A biblioteca `rand` possui vários tipos de PRNG disponíveis, como `StdRng` e `SmallRng`, que oferecem diferentes níveis de segurança e velocidade. Além disso, é possível fornecer uma semente para o gerador de números aleatórios, o que torna os resultados ainda mais imprevisíveis.

## Veja também

- Documentação oficial da biblioteca `rand`: https://crates.io/crates/rand
- Exemplo de uso da geração de números aleatórios em um jogo em Rust: https://blog.logrocket.com/create-a-game-in-rust-and-ggez-part-1/
- Mais informações sobre geradores de números pseudoaleatórios: https://docs.rs/rand/latest/rand/trait.Rng.html