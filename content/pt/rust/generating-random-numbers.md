---
title:                "Gerando números aleatórios"
html_title:           "Rust: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Gerar números aleatórios é uma técnica utilizada em programação para criar valores não previsíveis e imprevisíveis. Os programadores frequentemente utilizam essa técnica para executar jogos, simulações e criptografia. 

## Como fazer:
Gerar números aleatórios em Rust é muito simples! Basta utilizar a biblioteca padrão `rand` e seguir alguns passos:

- Importe a biblioteca com `use rand::prelude::*;`
- Crie um gerador de números aleatórios com `let mut rng = rand::thread_rng();`
- Utilize o gerador para gerar um número inteiro com `let random_number = rng.gen::<i32>();`.

Pronto! Agora você pode utilizar o número gerado em seu código.

## Mergulho Profundo:
A geração de números aleatórios é um assunto amplamente discutido na área da computação. Desde 1946, quando o ENIAC foi o primeiro computador a utilizar geração de números aleatórios, diversas técnicas e algoritmos têm sido desenvolvidos para gerar números verdadeiramente aleatórios. Em Rust, a biblioteca `rand` utiliza o algoritmo Mersenne Twister para gerar números pseudoaleatórios. Existem também outros algoritmos, como o Blum Blum Shub, que são mais seguros para uso em criptografia.

## Veja também:
- Documentação oficial da biblioteca `rand`: https://docs.rs/rand/0.7.3/rand/index.html
- Exemplos práticos de geração de números aleatórios em Rust: https://rust-random.github.io/book/intro.html