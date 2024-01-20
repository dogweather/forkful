---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que & por quê?

Gerar números aleatórios é a criação de valores (números) sem um padrão previsível ou discernível. Programadores fazem isso para criar diversidade indeterminada em seus programas, muitas vezes para o teste ou simulação.

## Como fazer:

Para gerar um número aleatório em Gleam, você precisará primeiro importar o módulo `gleam/otp/erlang`.

```Gleam
import gleam/otp/erlang

...

fn main(_) {
    // Gerar número inteiro aleatório entre 0 e 100
    let num = erlang:random_uniform(101)

    // Imprimir o número aleatório
    io.println(num)

    Ok(Nil)
}
```

Executar o código acima fornecerá um número aleatório no intervalo de [0, 100].

## Mergulho profundo

Historicamente, a geração de números aleatórios é um problema em computação. Os computadores são projetados para serem previsíveis, mas a aleatoriedade é imprevisível - portanto, os computadores geram números "pseudo-aleatórios".

Em Gleam, você usa a função `erlang:random_uniform/1` do módulo Erlang. Este é um número pseudo-aleatório gerador, pois depende do estado anterior para calcular o próximo número.

Alternativamente, você pode usar o módulo `rand` em Erlang se precisar de geradores aleatórios mais sofisticados, como normalmente distribuídos ou exponencialmente distribuídos.

Em termos de implementação, Gleam simplesmente acessa as bibliotecas de Erlang subjacentes. Isso significa que você tem acesso a uma gama completa de ferramentas para gerar números aleatórios conforme necessário.

## Veja também

- Introdução ao módulo Erlang `rand`: [https://erlang.org/doc/man/rand.html](https://erlang.org/doc/man/rand.html)
- Guia detalhado para geradores de números aleatórios: [https://prng.di.unimi.it/](https://prng.di.unimi.it/)