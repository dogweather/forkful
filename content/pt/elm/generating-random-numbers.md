---
title:    "Elm: Gerando números aleatórios"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em Elm?

Gerar números aleatórios pode ser útil em muitas situações diferentes. Por exemplo, se você está criando um jogo, pode usar números aleatórios para criar diferentes cenários e desafios para os jogadores. Além disso, gerar números aleatórios também pode ser útil em outras aplicações, como sorteios ou simulações.

## Como fazer isso em Elm?

Para gerar números aleatórios em Elm, você precisará importar o módulo `Random`. Ele contém funções úteis para criar e manipular valores aleatórios. Vamos dar uma olhada em um exemplo simples:

```Elm
import Random

randomNumber : Int
randomNumber =
    Random.int 1 10
```

Neste exemplo, importamos o módulo `Random` e criamos uma função chamada `randomNumber` que retorna um número inteiro aleatório entre 1 e 10. Podemos então chamar essa função em qualquer lugar do nosso código para obter um número aleatório:

```Elm
main =
    randomNumber -- retorna 7
```

Se quisermos gerar um número aleatório com decimais, podemos usar a função `Random.float` em vez de `Random.int`. É importante lembrar que os números aleatórios em Elm são gerados a partir de uma "semente" (seed) que pode ser configurada usando a função `Random.initialSeed`.

## Mergulho profundo

Agora que sabemos como gerar números aleatórios em Elm, podemos nos aprofundar um pouco mais. Quando usamos a função `Random.int` ou `Random.float`, podemos especificar um intervalo de valores a partir do qual o número aleatório será gerado. No entanto, se quisermos gerar valores aleatórios de tipos mais complexos, como uma lista de números inteiros, podemos usar a função `Random.list`:

```Elm
Random.list 5 (Random.int 1 10) -- retorna uma lista com 5 números inteiros aleatórios entre 1 e 10
```

Também podemos usar a função `Random.map` para modificar o valor gerado por uma função de números aleatórios. Por exemplo, podemos usar `Random.map` para gerar uma lista de valores booleanos a partir de uma lista de números inteiros gerados aleatoriamente:

```Elm
Random.map (\n -> n % 2 == 0) (Random.list 5 (Random.int 1 10)) -- retorna uma lista com 5 valores booleanos aleatórios
```

## Veja também

- [Documentação do módulo Random em Elm](https://package.elm-lang.org/packages/elm/random/latest/)
- [Vídeo tutorial sobre geração de números aleatórios em Elm](https://www.youtube.com/watch?v=7fAU1ps5tQE)
- [Exemplos de código para gerar números aleatórios em Elm](https://elmprogramming.com/random-numbers.html)