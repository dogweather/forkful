---
title:    "Haskell: Geração de números aleatórios"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em Haskell?

Gerar números aleatórios é uma tarefa comum em muitos programas e linguagens de programação. Em Haskell, podemos usar funções nativas e bibliotecas específicas para gerar números aleatórios. Este processo pode ser útil para jogos, simulações, criptografia, entre outros.

## Como fazer

Para gerar números aleatórios em Haskell, precisamos importar a biblioteca `System.Random`. Esta biblioteca possui uma função chamada `randomR` que recebe como parâmetro um intervalo e retorna um número aleatório dentro deste intervalo.

Vamos dar uma olhada em um exemplo simples de como usar essa função:

```Haskell
import System.Random

n <- randomRIO (1, 10)
print n -- output: 5
```

Neste exemplo, importamos a função `randomRIO` da biblioteca `System.Random`. Em seguida, definimos uma variável `n` que irá receber o valor retornado pela função ao gerar um número aleatório entre 1 e 10. Por fim, imprimimos o valor de `n` na tela.

Além da função `randomR`, a biblioteca `System.Random` também possui outras funções úteis para gerar números aleatórios em Haskell, como `random`, `randomIO` e `randomRs`. Cada uma dessas funções tem seus próprios parâmetros e funcionalidades, portanto é importante ler a documentação da biblioteca para utilizá-las corretamente.

## Aprofundando-se

A geração de números aleatórios em Haskell é baseada em geradores de números pseudoaleatórios (PRNGs). Isso significa que os números gerados não são verdadeiramente aleatórios, mas sim determinados por um algoritmo matemático que utiliza uma semente (seed) como entrada.

É importante lembrar que, ao executar um programa com geração de números aleatórios, os resultados podem variar a cada execução, dependendo da semente utilizada. No entanto, é possível definir uma semente específica para obter os mesmos números aleatórios em diferentes execuções do programa, o que é útil para fins de testes e depuração.

Além disso, é possível controlar o comportamento dos geradores de PRNGs em relação à geração de números repetidos ou sequências previsíveis. Para isso, podemos utilizar a função `setStdGen` para definir uma nova semente e alterar o estado do gerador.

## Veja também
- [Documentação da biblioteca System.Random](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [Tutorial sobre geração de números aleatórios em Haskell](https://wiki.haskell.org/Random)
- [Artigo sobre geradores de números pseudoaleatórios](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)