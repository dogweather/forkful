---
title:    "Gleam: Gerando números aleatórios"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que utilizar geração de números aleatórios?

A geração de números aleatórios é uma ferramenta importante na programação, sendo utilizada para simulações, jogos e muitas outras aplicações que requerem elementos imprevisíveis. Em Gleam, é possível gerar números aleatórios usando a biblioteca padrão "random".

## Como fazer:

Para gerar um número aleatório em Gleam, é necessário importar a biblioteca "random" e chamar a função "int" ou "float", dependendo do tipo de número desejado. Veja abaixo um exemplo para gerar um número inteiro entre 1 e 100:

```Gleam
import random

let numero = random.int(1, 100)

// output: 42
```

Também é possível definir uma semente (seed) para controlar os números gerados. Isso pode ser útil para reproduzir os mesmos resultados em diferentes execuções do programa. Veja um exemplo:

```Gleam
import random

let seed = 1234

// Gerar um número aleatório entre 1 e 100
let numero1 = random.int(1, 100)

// Gerar outro número aleatório usando a mesma semente
let numero2 = random.int(1, 100)

// output: 94 e 94 (os dois números são iguais)
```

## Mergulho profundo:

Além dos números inteiros e de ponto flutuante, a biblioteca "random" em Gleam também oferece funções para gerar strings, listas e até mesmo objetos aleatórios. Essas funções podem ser úteis para testes ou para criar dados de forma dinâmica. Você pode encontrar uma lista completa dessas funções na documentação oficial do Gleam.

Além disso, é importante lembrar que os números gerados por computadores nunca são realmente aleatórios, mas sim pseudoaleatórios, ou seja, seguem um algoritmo previsível. Por isso, é importante tomar cuidado ao utilizar números aleatórios em contextos sensíveis, como criptografia, por exemplo.

## Veja também:

- [Documentação da biblioteca "random" em Gleam](https://gleam.run/modules/standard_library/random.html)
- [Guia de programação em Gleam para iniciantes](https://intro-to-gleam.rs/)
- [Fórum da comunidade Gleam](https://gleam.run/forums/)