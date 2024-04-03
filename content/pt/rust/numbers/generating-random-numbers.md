---
date: 2024-01-27 20:35:24.579832-07:00
description: "Gerar n\xFAmeros aleat\xF3rios em Rust envolve o uso de bibliotecas\
  \ para produzir valores num\xE9ricos imprevis\xEDveis, o que \xE9 indispens\xE1\
  vel para tarefas que v\xE3o\u2026"
lastmod: '2024-03-13T22:44:46.363662-06:00'
model: gpt-4-0125-preview
summary: "Gerar n\xFAmeros aleat\xF3rios em Rust envolve o uso de bibliotecas para\
  \ produzir valores num\xE9ricos imprevis\xEDveis, o que \xE9 indispens\xE1vel para\
  \ tarefas que v\xE3o desde criptografia e simula\xE7\xF5es at\xE9 jogos e algoritmos\
  \ randomizados."
title: "Gera\xE7\xE3o de n\xFAmeros aleat\xF3rios"
weight: 12
---

## O Que & Por Que?

Gerar números aleatórios em Rust envolve o uso de bibliotecas para produzir valores numéricos imprevisíveis, o que é indispensável para tarefas que vão desde criptografia e simulações até jogos e algoritmos randomizados.

## Como fazer:

Rust depende de crates externas para a geração de números aleatórios, sendo `rand` a mais comumente usada. Para começar a gerar números aleatórios, você primeiro precisa adicionar `rand` ao seu arquivo `Cargo.toml`:

```toml
[dependencies]
rand = "0.8.5"
```

Em seguida, você pode gerar números aleatórios usando `rand` no seu código Rust. Aqui está um exemplo de geração de um número inteiro aleatório e um número de ponto flutuante:

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    
    // Gera um número inteiro aleatório entre 1 e 10
    let random_int: i32 = rng.gen_range(1..=10);
    println!("Número Inteiro Aleatório: {}", random_int);
    
    // Gera um número de ponto flutuante aleatório entre 0.0 e 1.0
    let random_float: f64 = rng.gen::<f64>();
    println!("Número de Ponto Flutuante Aleatório: {}", random_float);
}
```

Um exemplo de saída pode ser:

```plaintext
Número Inteiro Aleatório: 7
Número de Ponto Flutuante Aleatório: 0.9401077112175732
```

Note que rodar o programa novamente produzirá valores diferentes.

## Mergulho Profundo

A geração de números aleatórios em Rust, facilitada pelo `rand` e suas dependências como `getrandom`, representa uma ampla abstração sobre as facilidades do sistema operacional e geradores algorítmicos. Historicamente, a aleatoriedade em computação evoluiu de algoritmos simples e previsíveis para métodos complexos e cryptographicamente seguros. A abordagem do Rust encapsula essa evolução por meio de seu trait `Rng` plugável, que pode ser suportado por vários geradores de acordo com a qualidade de aleatoriedade e desempenho requeridos.

Para a maioria das aplicações, depender do `rand` e do RNG do sistema oferece um bom equilíbrio entre simplicidade e entropia. No entanto, para aplicações criptográficas, crates como `rand` deferem para `getrandom` para a semente, que por sua vez depende de mecanismos específicos do SO (por exemplo, `/dev/urandom` em sistemas semelhantes ao Unix), garantindo aleatoriedade segura do ponto de vista criptográfico.

Alternativamente, se você tem necessidades específicas não atendidas pelo `rand`, explorar outros crates ou implementar geradores personalizados baseados em modelos matemáticos pode ser um caminho. No entanto, para a grande maioria dos casos de uso, `rand` e seu ecossistema fornecem soluções robustas que são eficientes e fáceis de integrar em aplicações Rust.
