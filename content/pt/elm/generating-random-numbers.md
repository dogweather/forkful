---
title:                "Elm: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Por que gerar números aleatórios é importante em programação Elm?

Gerar números aleatórios é uma ferramenta poderosa em programação Elm. Quando trabalhamos com dados dinâmicos, ter a capacidade de gerar valores aleatórios pode ser extremamente útil. Isso pode ser usado para simular dados, criar testes de unidade e até mesmo adicionar elementos de sorte a um aplicativo. Neste artigo, discutiremos como gerar números aleatórios em Elm e como isso pode melhorar nossos projetos.

## Como fazer isso em Elm

Em Elm, podemos usar a função `random` do módulo `Random` para gerar números aleatórios. Primeiro, precisamos importar o módulo em nosso arquivo Elm:

```
import Random
```

A seguir, podemos definir a função `random` com o tipo de dados que queremos gerar, uma faixa de valores possíveis e a semente aleatória:

```
random : Random.Generator Int
random =
    Random.int 1 10
```

Neste exemplo, estamos gerando um número inteiro aleatório entre 1 e 10. Podemos então usar a função `generate` para obter o valor real:

```
Random.generate MyMessage random
```

Podemos definir a mensagem `MyMessage` para manipular o resultado do número aleatório.

## Profundidade de gerar números aleatórios em Elm

Ao trabalhar com geradores aleatórios em Elm, é importante entender como a semente aleatória funciona. Uma semente é um valor que é usado para gerar uma sequência de números aleatórios. Isso significa que se usarmos a mesma semente, sempre obteremos a mesma sequência de números. Portanto, é importante atualizar a semente com cada uso para garantir que cada vez que o programa seja executado, os números aleatórios sejam diferentes.

Também é importante notar que, ao usar a função `random`, temos a garantia de que o resultado será um número realmente aleatório, independentemente do usuário ou do estado do aplicativo. Isso garante a consistência e a confiabilidade dos dados em nossos projetos.

# Veja também

- Documentação do módulo Elm Random: https://package.elm-lang.org/packages/elm/random/latest
- Tutorial Elm Random: https://dev.to/kaybe/remotely-icymi-learning-how-to-elm-random-442e