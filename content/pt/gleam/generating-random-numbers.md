---
title:                "Gleam: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios?

Gerar números aleatórios é uma tarefa comumente realizada em programação para uma variedade de propósitos. Esses números podem ser usados em jogos, simulações, criptografia e muitas outras aplicações. A capacidade de gerar números aleatórios é uma habilidade importante para qualquer programador.

## Como fazer:

Gerar números aleatórios em Gleam é uma tarefa fácil e pode ser feita de diversas maneiras. Uma abordagem simples é usar a função `rand.int` para gerar um número inteiro aleatório em um intervalo específico. Por exemplo:

````Gleam
let num = rand.int(1, 10)
````

Isso irá gerar um número aleatório entre 1 e 10, incluindo ambos os números.

Outra opção é usar a função `rand.float` para gerar um número decimal aleatório em um intervalo específico. Por exemplo:

````Gleam
let num = rand.float(1.0, 10.0)
````

Isso irá gerar um número aleatório entre 1.0 e 10.0.

Existem também outras funções disponíveis, como `rand.bool` para gerar valores booleanos aleatórios e `rand.bytes` para gerar uma lista de bytes aleatórios.

## Mergulho Profundo:

Para uma compreensão mais profunda de como a geração de números aleatórios funciona em Gleam, é importante entender que essa função é baseada em um gerador de números pseudoaleatórios que utiliza uma semente (seed) para gerar os números. A semente é um valor inicial que é usado como base para a geração de números. Por padrão, o gerador de números em Gleam usa a hora atual como semente, mas é possível especificar uma semente manualmente usando a função `rand.with_seed`.

Além disso, o módulo `Rand` em Gleam também oferece funções para gerar números aleatórios com diferentes distribuições, como a Distribuição Normal, a Distribuição de Poisson e a Distribuição Binomial.

## Veja também:

Para saber mais sobre a geração de números aleatórios em Gleam e como aplicá-la em suas próprias aplicações, confira os recursos abaixo:

- Documentação oficial da função `Rand`: https://gleam.run/modules/gleam_std.Rand.html
- Tutorial sobre números aleatórios em Gleam: https://dev.to/gleam_tuts/how-to-generate-random-numbers-in-gleam-2nh1
- Exemplos de uso de geração de números aleatórios em Gleam: https://github.com/gleam-lang/gleam_stdlib/blob/master/test/rand_test.gleam