---
title:                "Gerando números aleatórios"
html_title:           "Elm: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios?

Se você está desenvolvendo um aplicativo de jogo, sorteio ou qualquer outro sistema que envolva aleatoriedade, é essencial que você saiba como gerar números aleatórios. Além disso, a geração de números aleatórios é uma habilidade fundamental em programação e pode ser útil em vários outros contextos.

## Como fazer isso em Elm

Gerar números aleatórios em Elm é muito simples e pode ser feito usando a função `Random.generate` e passando um gerador adequado como argumento. Por exemplo, se quisermos gerar um número aleatório entre 1 e 10, podemos usar o seguinte código:

```Elm
module Main exposing (..)

import Random exposing (generate, int)

main =
  Random.generate NewNumber (int 1 10)


type Msg = 
  NewNumber Int


update msg model =
  case msg of 
    NewNumber num ->
      -- Faz alguma coisa com o número gerado
      ( model, Cmd.none )


```

A saída da aplicação sera um "Msg" que pode ser usado para atualizar o estado do aplicativo ou realizar outras ações necessárias.

## Mergulho Profundo

Em Elm, os geradores são um tipo de dados que representa um processo de geração de valores aleatórios. Eles são compostos por funções que geram valores e podem ser combinados usando funções como `map` e `andThen` para criar geradores mais complexos. Além disso, é importante ter em mente que Elm tem um gerenciamento de estado imutável e, portanto, os geradores sempre retornam um novo gerador ao invés de alterar o gerador existente.

## Veja também

- Documentação oficial de Elm sobre geradores aleatórios: https://guide.elm-lang.org/effects/random.html
- Exemplos de uso de geradores aleatórios em projetos Elm: https://github.com/simonh1000/elm-random-example
- Vídeo tutorial sobre geração de números aleatórios em Elm:  https://www.youtube.com/watch?v=04xJhTT5M4M